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

#include <SDL_events.h>

#include "command.h"
#include "render.h"
#include "empic.h"
#include "event.h"

static void process_window_event(SDL_Event * event)
{
  switch (event->window.event)
    {
    case SDL_WINDOWEVENT_SHOWN:
    case SDL_WINDOWEVENT_EXPOSED:
    case SDL_WINDOWEVENT_MAXIMIZED:
    case SDL_WINDOWEVENT_RESTORED:
    case SDL_WINDOWEVENT_RESIZED:
    case SDL_WINDOWEVENT_SIZE_CHANGED:
      update_viewport();
    }
}

static void process_key_event(SDL_Event * event)
{
  switch (event->key.keysym.sym)
    {
    case SDLK_q:
      empic_quit();
      break;
    case SDLK_EQUALS:
    case SDLK_KP_PLUS:
      zoom_view_frac(1.1f);
      break;
    case SDLK_MINUS:
    case SDLK_KP_MINUS:
      zoom_view_frac(0.9f);
      break;
    case SDLK_0:
      zoom_view_fit(ZOOM_FIT_BIG);
      break;
    case SDLK_9:
      zoom_view_fit(ZOOM_FIT_SMALL);
      break;
    case SDLK_1:
      zoom_view(1.0f);
      break;
    case SDLK_RIGHT:
      move_view_delta(10, 0);
      break;
    case SDLK_LEFT:
      move_view_delta(-10, 0);
      break;
    case SDLK_UP:
      move_view_delta(0, 10);
      break;
    case SDLK_DOWN:
      move_view_delta(0, -10);
      break;
    case SDLK_SLASH:
      rotate_view(0);
      break;
    case SDLK_COMMA:
      rotate_view(-90);
      break;
    case SDLK_PERIOD:
      rotate_view(90);
      break;
    case SDLK_SEMICOLON:
      rotate_view_delta(-10);
      break;
    case SDLK_QUOTE:
      rotate_view_delta(10);
      break;
    case SDLK_SPACE:
      load_next_image();
      zoom_view_fit(ZOOM_FIT_BIG);
      break;
    case SDLK_BACKSPACE:
      load_prev_image();
      zoom_view_fit(ZOOM_FIT_BIG);
      break;
    default:;
    }

  update_render();
}

static void process_event(SDL_Event * event)
{
  if (event->type == SDL_KEYDOWN)
    process_key_event(event);
}

static void process_event_emacs(SDL_Event * event)
{
  if (event->type == command_event_id)
    exec_command((struct cmdarg *)event->user.data1);
  else if (event->type == SDL_KEYDOWN)
    /* TODO Send the key event to Emacs */
    ;
}

static void (*process_event_fn)(SDL_Event * event) = process_event;

void wait_event()
{
  SDL_Event event;
  if (!SDL_WaitEvent(&event))
    return;

  switch (event.type)
    {
    case SDL_QUIT:
      empic_quit();
      break;
    case SDL_WINDOWEVENT:
      process_window_event(&event);
      break;
    default:
      process_event_fn(&event);
    }
}

void set_emacs_mode(int mode)
{
  process_event_fn = mode ? process_event_emacs : process_event;
}
