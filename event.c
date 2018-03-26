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
#include <SDL_log.h>

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

/* SDL to Emacs key names */
static struct sdl_to_emacs_t
{
  const char * sdl, * emacs;
} sdl_to_emacs[74];

static void init_sdl_to_emacs()
{
  struct sdl_to_emacs_t * p = sdl_to_emacs;

#define SET(CODE, KEY)                                                  \
  p->sdl = SDL_GetScancodeName(SDL_SCANCODE_ ## CODE); p->emacs = KEY; p++

  SET(A, "a");
  SET(B, "b");
  SET(C, "c");
  SET(D, "d");
  SET(E, "e");
  SET(F, "f");
  SET(G, "g");
  SET(H, "h");
  SET(I, "i");
  SET(J, "j");
  SET(K, "k");
  SET(L, "l");
  SET(M, "m");
  SET(N, "n");
  SET(O, "o");
  SET(P, "p");
  SET(Q, "q");
  SET(R, "r");
  SET(S, "s");
  SET(T, "t");
  SET(U, "u");
  SET(V, "v");
  SET(W, "w");
  SET(X, "x");
  SET(Y, "y");
  SET(Z, "z");
  SET(RETURN, "<return>");
  SET(ESCAPE, "<escape>");
  SET(BACKSPACE, "<backspace>");
  SET(TAB, "<tab>");
  SET(SPACE, "<space>");
  SET(CAPSLOCK, "CapsLock");
  SET(F1, "<f1>");
  SET(F2, "<f2>");
  SET(F3, "<f3>");
  SET(F4, "<f4>");
  SET(F5, "<f5>");
  SET(F6, "<f6>");
  SET(F7, "<f7>");
  SET(F8, "<f8>");
  SET(F9, "<f9>");
  SET(F10, "<f10>");
  SET(F11, "<f11>");
  SET(F12, "<f12>");
  SET(PRINTSCREEN, "<print>");
  SET(SCROLLLOCK, "<scroll_lock>");
  SET(PAUSE, "<pause>");
  SET(INSERT, "<insert>");
  SET(HOME, "<home>");
  SET(PAGEUP, "<prior>");
  SET(DELETE, "<delete>");
  SET(END, "<end>");
  SET(PAGEDOWN, "<next>");
  SET(RIGHT, "<right>");
  SET(LEFT, "<left>");
  SET(DOWN, "<down>");
  SET(UP, "<up>");
  SET(NUMLOCKCLEAR, "<numlock>");
  SET(KP_DIVIDE, "<kp-divide>");
  SET(KP_MULTIPLY, "<kp-multiply>");
  SET(KP_MINUS,"<kp-subtract>");
  SET(KP_PLUS,"<kp-add>");
  SET(KP_ENTER,"<kp-enter>");
  SET(KP_1, "<kp-1>");
  SET(KP_2, "<kp-2>");
  SET(KP_3, "<kp-3>");
  SET(KP_4, "<kp-4>");
  SET(KP_5, "<kp-5>");
  SET(KP_6, "<kp-6>");
  SET(KP_7, "<kp-7>");
  SET(KP_8, "<kp-8>");
  SET(KP_9, "<kp-9>");
  SET(KP_0, "<kp-0>");
  SET(KP_PERIOD, "<kp-decimal>");

#undef SET
}

static void process_event_emacs(SDL_Event * event)
{
  if (event->type == command_event_id)
    exec_command((char **)event->user.data1);
  else if (event->type == SDL_KEYDOWN)
    {
      int i;
      const char * name;
      static char mod[10];
        mod[0] = '\0';

      if (event->key.keysym.scancode >= SDL_SCANCODE_LCTRL &&
          event->key.keysym.scancode <= SDL_SCANCODE_RGUI)
        return;

      if (event->key.keysym.mod & KMOD_CTRL)
        strcat(mod, "C-");
      if (event->key.keysym.mod & KMOD_GUI)
        strcat(mod, "M-");
      if (event->key.keysym.mod & KMOD_ALT)
        strcat(mod, "A-");
      if (event->key.keysym.mod & KMOD_SHIFT)
        strcat(mod, "S-");

      name = SDL_GetScancodeName(event->key.keysym.scancode);
      for (i = 0; i < sizeof(sdl_to_emacs) / sizeof(struct sdl_to_emacs_t); i++)
        if (name == sdl_to_emacs[i].sdl)
          {
            name = sdl_to_emacs[i].emacs;
            break;
          }

      printf("%s%s\n", mod, name);
      fflush(stdout);
    }
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

  if (mode)
    init_sdl_to_emacs();
}
