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

#ifndef COMMAND_H
#define COMMAND_H

#include <SDL_types.h>

#define COMMAND_NAME_SIZE 32

typedef int (*cmdfn)(char * cmd, char ** args);

struct command
{
  char name[COMMAND_NAME_SIZE];
  cmdfn fn;
};

extern Uint32 command_event_id;

float argtof(char * arg, int * error);

void register_command(char * name, cmdfn fn);

int start_read_commands();
void stop_read_commands();

int exec_command(char ** args);

#define ASSERT_ARGS(CMD, ARGS, MSG)                             \
  if (!(ARGS)[0])                                               \
    {                                                           \
      SDL_Log("%s: " MSG, CMD);                                 \
      return -1;                                                \
    }

#define ASSERT_ARGS1(CMD, ARGS)                                 \
  ASSERT_ARGS(CMD, ARGS, "the command requires an argument")

#define ASSERT_ARGS2(CMD, ARGS)                                 \
  if (!(ARGS)[0] || !(ARGS)[1])                                 \
    {                                                           \
      SDL_Log("%s: the command requires two arguments", CMD);   \
      return -1;                                                \
    }

#endif /* COMMAND_H */
