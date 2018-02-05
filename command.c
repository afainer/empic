/* Copyright (C) 2016-2018 Andrey Fainer <fandrey@gmx.com>

This file is part of Empic.

Empic is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

Empic is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Empic.  If not, see <http://www.gnu.org/licenses/>. */

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <SDL_events.h>
#include <SDL_thread.h>
#include <SDL_log.h>

#include "command.h"

struct command commands_table[256];
size_t commands_num = 0;
FILE * command_stream = NULL;
Uint32 command_event_id;

void register_command(char * name, cmdfn fn)
{
  assert(commands_num < sizeof(commands_table) / sizeof(struct command));

  strncpy(commands_table[commands_num].name, name, COMMAND_NAME_SIZE);
  commands_table[commands_num++].fn = fn;
}

#define MAX_ARGS 255

static struct cmdarg * read_cmd()
{
  static char str[2048];
  static struct cmdarg args[MAX_ARGS + 1];

  char * p = fgets(str, sizeof(str), command_stream);
  ssize_t n;

  if (!p)
    return NULL;

  n = strlen(p);
  if (n >= 1 && p[n - 1] == '\n')
    p[n - 1] = '\0';
  if (n >= 2 && p[n - 2] == '\r')
    p[n - 2] = '\0';

  n = 1;
  args[0].type = STRING;
  args[0].s = p = strtok(str, " \t");

  while((p = strtok(NULL, " \t")) && n <= MAX_ARGS)
    {
      char * end;

      args[n].f = strtof(p, &end);

      if (p == end || isspace(*end))
        {
          args[n].type = STRING;
          args[n].s = p;
        }
      else
        {
          args[n].type = FLOAT;
          if (errno == ERANGE)
            {
              const char
                *err = args[n].f == HUGE_VALF || args[n].f == -HUGE_VALF ?
                "over" : "under";

              SDL_Log("read_cmd: the argument %s causes %sflow", p, err);
            }
        }

      n++;
    }

  if (p && n > MAX_ARGS)
    {
      SDL_Log("%s: exceeded maximum number (%d) of arguments",
              args[0].s, MAX_ARGS);
      n = MAX_ARGS + 1;
    }

  args[n].type = STRING;
  args[n].s = NULL;

  return args;
}

static int thread_started, stop_thread, command_is_executing;
static SDL_mutex * mutex;
static SDL_cond * cond;

static int thread_fun(void *data)
{
  SDL_Event event;

  mutex = SDL_CreateMutex();
  cond  = SDL_CreateCond();
  command_is_executing = 0;

  while (!stop_thread)
    {
      struct cmdarg * args = read_cmd();

      if (!args || !args[0].s)
        continue;

      event.type = command_event_id;
      event.user.data1 = args;

      SDL_LockMutex(mutex);

      command_is_executing = 1;
      SDL_PushEvent(&event);

      while (command_is_executing && !stop_thread)
        SDL_CondWait(cond, mutex);

      SDL_UnlockMutex(mutex);
    }

  SDL_DestroyMutex(mutex);
  SDL_DestroyCond(cond);

  return 0;
}

int start_read_commands()
{
  SDL_Thread * thread;

  if (thread_started)
    {
      SDL_Log("Command thread is already started");
      return -1;
    }

  command_event_id = SDL_RegisterEvents(1);

  if (command_event_id == (Uint32) - 1)
    {
      SDL_Log("Failed to register the command event");
      return -1;
    }

  command_stream = stdin;
  stop_thread = 0;

  if (!(thread = SDL_CreateThread(thread_fun, "command", NULL)))
    return -1;

  SDL_DetachThread(thread);
  thread_started = 1;

  return 0;
}

void stop_read_commands()
{
  stop_thread = 1;
}

int exec_command(struct cmdarg * args)
{
  int i, r;

  for(i = 0; i < commands_num; i++)
    if (!strcmp(args[0].s, commands_table[i].name))
      break;

  if (i < commands_num)
    r = commands_table[i].fn(commands_table[i].name, args + 1);
  else
    {
      SDL_Log("%s: unknown command", args[0].s);
      r = -1;
    }

  SDL_LockMutex(mutex);
  command_is_executing = 0;
  SDL_CondSignal(cond);
  SDL_UnlockMutex(mutex);

  return r;
}
