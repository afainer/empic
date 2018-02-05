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

#include <string.h>
#include <SDL2/SDL.h>

#include "command.h"
#include "render.h"
#include "event.h"
#include "empic.h"

SDL_Window * empic_window = NULL;

static int quit = 0;

static struct
{
  char ** names;
  int size, current;
} files_list = {NULL, 0, -1};

static int create_window()
{
  empic_window = SDL_CreateWindow("Empic", 0, 0, 800, 600,
                                  SDL_WINDOW_OPENGL |
                                  SDL_WINDOW_SHOWN |
                                  SDL_WINDOW_RESIZABLE);

  if (!(empic_window && SDL_GL_CreateContext(empic_window)))
    return 0;

  return 1;
}

static void destroy_window()
{
  if (SDL_GL_GetCurrentContext())
    SDL_GL_DeleteContext(SDL_GL_GetCurrentContext());

  if (empic_window)
    SDL_DestroyWindow(empic_window);
}

static void cleanup_and_quit()
{
  stop_read_commands();
  destroy_window();
  SDL_Quit();
}

static void set_title(char * name)
{
  char str[256];
  snprintf(str, sizeof(str), "%s - Empic", name);
  SDL_SetWindowTitle(empic_window, str);
}

void load_next_image()
{
  int n = files_list.current;

  while (n < files_list.size - 1)
    if (load_image(files_list.names[++n]))
      {
        files_list.current = n;
        set_title(files_list.names[n]);
        return;
      }
}

void load_prev_image()
{
  int n = files_list.current;

  while (n > 0)
    if (load_image(files_list.names[--n]))
      {
        files_list.current = n;
        set_title(files_list.names[n]);
        return;
      }
}

int zoom_cmd(char * cmd, struct cmdarg * args)
{
  if (is_empty_arg(args))
    {
      SDL_Log("%s: the command requires an argument", cmd);
      return -1;
    }

  if (args[0].type == FLOAT)
    {
      zoom_view_frac(args[0].f);
      return 0;
    }

  if (args[0].type != STRING)
    {
      SDL_Log("%s: wrong argument type", cmd);
      return -1;
    }

  if (!strcmp(args[0].s, "in"))
    zoom_view_frac(1.1f);
  else if (!strcmp(args[0].s, "out"))
    zoom_view_frac(0.9f);
  else
    {
      SDL_Log("%s: wrong argument: `%s', should be `in' or `out'",
              cmd, args[0].s);
      return -1;
    }

  return 0;
}

void empic_quit()
{
  quit = 1;
}

int main(int argc, char ** argv)
{
  if (argc < 2)
    return 0;

  if (argv[1][0] == '-' && argv[1][1] == 'e')
    {
      register_command("zoom", zoom_cmd);
      start_read_commands();
      argv += 1;
      argc -= 1;
    }

  if (SDL_Init(SDL_INIT_VIDEO) < 0)
    {
      SDL_Log("Couldn't init: %s\n", SDL_GetError());
      cleanup_and_quit();
      return 1;
    }

  if (!create_window())
    {
      SDL_Log("Couldn't create window: %s\n", SDL_GetError());
      cleanup_and_quit();
      return 2;
    }

  if (!init_render())
    {
      cleanup_and_quit();
      return 3;
    }

  files_list.names = argv + 1;
  files_list.size = argc - 1;

  load_next_image();
  if (files_list.current < 0)
    {
      cleanup_and_quit();
      return 4;
    }

  /* Simulate keydown for ZOOM_FIT_BIG */
  SDL_Event key;
  key.type = SDL_KEYDOWN;
  key.key.keysym.sym = SDLK_0;
  SDL_PushEvent(&key);

  while (!quit)
    {
      if (!render())
        {
          cleanup_and_quit();
          return 5;
        }

      wait_event();
    }

  cleanup_and_quit();

  return 0;
}
