/* Copyright (C) 2016 Andrey Fainer <fandrey@gmx.com>

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

#include <SDL_types.h>

#define COMMAND_NAME_SIZE 32

struct cmdarg
{
  enum
    {
      STRING = 0,
      FLOAT,
    } type;

  union
  {
    char * s;
    float f;
  };
};

typedef int (*cmdfn)(char * cmd, struct cmdarg * args);

struct command
{
  char name[COMMAND_NAME_SIZE];
  cmdfn fn;
};

inline static int is_empty_arg(struct cmdarg * arg)
{
  return arg->type == STRING && !arg->s;
}

extern Uint32 command_event_id;

void register_command(char * name, cmdfn fn);

int start_read_commands();
void stop_read_commands();

int exec_command(struct cmdarg * args);
