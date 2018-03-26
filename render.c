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

#include <math.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_opengles2.h>
#include <SDL2/SDL_image.h>

#include "command.h"
#include "render.h"
#include "empic.h"

static GLuint shader_program = 0;
static const char * vertex_shader =
"attribute vec2 position;"
"attribute vec2 texposition;"
"uniform mat3 view;"
"varying vec2 texcoord;"
""
"void main()"
"{"
"  gl_Position = vec4(view * vec3(position, 1.), 1.);"
"  texcoord = texposition;"
"}";

static const char * fragment_shader =
"varying vec2 texcoord;"
"uniform sampler2D sampler;"
""
"void main()"
"{"
"  gl_FragColor = texture2D(sampler, texcoord);"
"}";

static const float tex_vertices[8] = {0.f, 1.f,
                                      1.f, 1.f,
                                      0.f, 0.f,
                                      1.f, 0.f};

static int attrib_position = -1;
static int attrib_texposition = -1;
static int uniform_view = -1;
static int uniform_sampler = -1;

#define GL_CHECK(x)                                             \
  x;                                                            \
  {                                                             \
    GLenum err = glGetError();                                  \
    if (err != GL_NO_ERROR)                                     \
      {                                                         \
        SDL_Log("glGetError() = %i (0x%.8x) at line %i\n",      \
                err,                                            \
                err,                                            \
                __LINE__);                                      \
        return 0;                                               \
      }                                                         \
  }

static struct
{
  float x, y, zoom, rotate;
} view;

void mat33mul(float * m, const float * l, const float * r)
{
  m[0] = l[0] * r[0] + l[1] * r[3] + l[2] * r[6];
  m[1] = l[0] * r[1] + l[1] * r[4] + l[2] * r[7];
  m[2] = l[0] * r[2] + l[1] * r[5] + l[2] * r[8];
  m[3] = l[3] * r[0] + l[4] * r[3] + l[5] * r[6];
  m[4] = l[3] * r[1] + l[4] * r[4] + l[5] * r[7];
  m[5] = l[3] * r[2] + l[4] * r[5] + l[5] * r[8];
  m[6] = l[6] * r[0] + l[7] * r[3] + l[8] * r[6];
  m[7] = l[6] * r[1] + l[7] * r[4] + l[8] * r[7];
  m[8] = l[6] * r[2] + l[7] * r[5] + l[8] * r[8];
}

void transform(const float * mat, float * point)
{
  float p[ 3 ];

  p[0] = mat[0] * point[0] + mat[3] * point[1] + mat[6];
  p[1] = mat[1] * point[0] + mat[4] * point[1] + mat[7];
  p[2] = mat[2] * point[0] + mat[5] * point[1] + mat[8];

  point[0] = p[0] / p[2];
  point[1] = p[1] / p[2];
}

float * rotation_matrix(int invert)
{
  static float rot[9];
  float
    c = cosf(invert ? -view.rotate : view.rotate),
    s = sinf(invert ? -view.rotate : view.rotate);

  rot[0] =   c; rot[1] =  -s; rot[2] = 0;
  rot[3] =   s; rot[4] =   c; rot[5] = 0;
  rot[6] = 0.f; rot[7] = 0.f; rot[8] = 1.f;

  return rot;
}

float * vertex_array(int width, int height)
{
  static float vertices[8];

  if (width > 0 || height > 0)
    {
      vertices[0] = -width / 2.f;
      vertices[1] = -height / 2.f;
      vertices[2] = width / 2.f;
      vertices[3] = -height / 2.f;
      vertices[4] = -width / 2.f;
      vertices[5] = height / 2.f;
      vertices[6] = width / 2.f;
      vertices[7] = height / 2.f;;
    }

  return vertices;
}

void bbox_size(float * width, float * height)
{
  float
    *va = vertex_array(0, 0),
    p1[2], p2[2], p3[2], p4[2],
    minx, maxx, miny, maxy;

  p1[0] = va[0];
  p1[1] = va[1];
  p2[0] = va[2];
  p2[1] = va[3];
  p3[0] = va[4];
  p3[1] = va[5];
  p4[0] = va[6];
  p4[1] = va[7];

  transform(rotation_matrix(0), p1);
  transform(rotation_matrix(0), p2);
  transform(rotation_matrix(0), p3);
  transform(rotation_matrix(0), p4);

  minx = SDL_min(p1[0], p2[0]);
  minx = SDL_min(minx,  p3[0]);
  minx = SDL_min(minx,  p4[0]);
  miny = SDL_min(p1[1], p2[1]);
  miny = SDL_min(miny,  p3[1]);
  miny = SDL_min(miny,  p4[1]);
  maxx = SDL_max(p1[0], p2[0]);
  maxx = SDL_max(maxx,  p3[0]);
  maxx = SDL_max(maxx,  p4[0]);
  maxy = SDL_max(p1[1], p2[1]);
  maxy = SDL_max(maxy,  p3[1]);
  maxy = SDL_max(maxy,  p4[1]);

  *width = maxx - minx;
  *height = maxy - miny;
}

void move_view(float x, float y)
{
  int w, h;
  float w2, h2, imagew, imageh;

  SDL_GetWindowSize(empic_window, &w, &h);
  bbox_size(&imagew, &imageh);

  w2 = w / 2.f / view.zoom;
  imagew /= 2.f;
  h2 = h / 2.f / view.zoom;
  imageh /= 2.f;

  if (view.x - w2 <= -imagew && view.x + w2 >= imagew)
    x = 0.f;
  else if (x - w2 < -imagew)
    x = -imagew + w2;
  else if (x + w2 > imagew)
    x = imagew - w2;

  if (view.y - h2 <= -imageh && view.y + h2 >= imageh)
    y = 0.f;
  else if (y - h2 < -imageh)
    y = -imageh + h2;
  else if (y + h2 > imageh)
    y = imageh - h2;

  view.x = x;
  view.y = y;
}

void move_view_delta(float x, float y)
{
  move_view(view.x + x, view.y + y);
}

void zoom_view(float z)
{
  if(z > 0.f)
    view.zoom = z;
}

void zoom_view_frac(float z)
{
  zoom_view(view.zoom * z);
}

void zoom_view_fit(zoom_fit_t fit)
{
  int w, h;
  float z = view.zoom, imagew, imageh;

  SDL_GetWindowSize(empic_window, &w, &h);

  bbox_size(&imagew, &imageh);

  switch (fit)
    {
    case ZOOM_FIT:
      break;

    case ZOOM_FIT_BIG:
      if (imagew <= w && imageh <= h)
        {
          view.zoom = 1.f;
          return;
        }
      break;

    case ZOOM_FIT_SMALL:
      if (imagew >= w || imageh >= h)
        return;
      break;

    default:
      SDL_Log("Wrong zoom fit type: %d", fit);
      return;
    }

  z = (float)w / imagew;
  if (h < imageh * z)
    z = (float)h / imageh;

  view.zoom = z;
  move_view(0.f, 0.f);
}

void rotate_view(float angle)
{
  view.rotate = angle * M_PI / 180.0;
}

void rotate_view_delta(float angle)
{
  view.rotate += angle * M_PI / 180.0;
}

void view_port(int * x, int * y, int * w, int * h)
{
  SDL_GetWindowSize(empic_window, w, h);

  if (*w > *h)
    {
      *x = 0;
      *y = (*h - *w) / 2;
      *h = *w;
    }
  else
    {
      *x = (*w - *h) / 2;
      *y = 0;
      *w = *h;
    }
}

void update_render();

void update_viewport()
{
  int x, y, w, h;
  view_port(&x, &y, &w, &h);
  glViewport(x, y, w, h);
  update_render();
}

void make_view_matrix(float * m)
{
  int x, y, w, h;
  float z, sc[9], tr[9], rs[9], pos[2] = {-view.x, -view.y};

  view_port(&x, &y, &w, &h);
  z = 2.f / w * view.zoom;

  /* Scale */
  sc[0] =   z; sc[1] = 0.f; sc[2] = 0.f;
  sc[3] = 0.f; sc[4] =   z; sc[5] = 0.f;
  sc[6] = 0.f; sc[7] = 0.f; sc[8] = 1.f;

  /* Transform the view position with inverted rotation, so the view
     is moving regardless of rotation. */
  transform(rotation_matrix(1), pos);

  /* Translation */
  tr[0] =    1.f; tr[1] =    0.f; tr[2] = 0.f;
  tr[3] =    0.f; tr[4] =    1.f; tr[5] = 0.f;
  tr[6] = pos[0]; tr[7] = pos[1]; tr[8] = 1.f;

  mat33mul(rs, sc, rotation_matrix(0));
  mat33mul(m, tr, rs);
}

int check_status(void(*getfn)(GLuint, GLenum, GLint*),
                 void(*logfn)(GLuint, GLsizei, GLsizei*, GLchar*),
                 GLuint id,
                 GLenum pname,
                 const char * errmsg)
{
  GLint stat;
  getfn(id, pname, &stat);
  if (stat != GL_TRUE)
    {
      GLsizei loglen;
      char log[1024];

      logfn(id, sizeof( log ), &loglen, log);
      SDL_Log("%s%s", errmsg, log);
      return 0;
    }

  return 1;
}

int use_shader()
{
  GLuint sh;

  shader_program = GL_CHECK(glCreateProgram());

  sh = GL_CHECK(glCreateShader(GL_VERTEX_SHADER));

  GL_CHECK(glShaderSource(sh, 1, &vertex_shader, NULL));
  GL_CHECK(glCompileShader(sh ));
  if (!check_status(glGetShaderiv,
                    glGetShaderInfoLog,
                    sh,
                    GL_COMPILE_STATUS,
                    "Vertex shader compilation failed:\n"))
    return 0;

  GL_CHECK(glAttachShader(shader_program, sh));

  sh = GL_CHECK(glCreateShader(GL_FRAGMENT_SHADER));

  GL_CHECK(glShaderSource(sh, 1, &fragment_shader, NULL));
  GL_CHECK(glCompileShader(sh ));
  if (!check_status(glGetShaderiv,
                    glGetShaderInfoLog,
                    sh,
                    GL_COMPILE_STATUS,
                    "Fragment shader compilation failed:\n"))
    return 0;

  GL_CHECK(glAttachShader(shader_program, sh));

  GL_CHECK(glLinkProgram(shader_program));
  if (!check_status(glGetProgramiv,
                    glGetProgramInfoLog,
                    shader_program,
                    GL_LINK_STATUS,
                    "Shader program link failed:\n"))
    return 0;

  attrib_position =
    GL_CHECK(glGetAttribLocation(shader_program, "position"));
  attrib_texposition =
    GL_CHECK(glGetAttribLocation(shader_program, "texposition"));
  uniform_view =
    GL_CHECK(glGetUniformLocation(shader_program, "view"));
  uniform_sampler =
    GL_CHECK(glGetUniformLocation(shader_program, "sampler"));

  GL_CHECK(glUseProgram(shader_program));

  GL_CHECK(glUniform1i(uniform_sampler, 0));

  return 1;
}

int load_texture(SDL_Surface * surface)
{
  static GLuint texture = -1;

  if (!surface)
    {
      texture = -1;
      return 1;
    }

  if (surface->format->format != SDL_PIXELFORMAT_RGB24)
    {
      SDL_Log("Surface pixel fromat is 0x%x,"
              " but only SDL_PIXELFORMAT_RGB24, 0x%x is supported.",
              surface->format->format, SDL_PIXELFORMAT_RGB24);
      return 0;
    }

  if (texture >= 0)
    {
      GL_CHECK(glDeleteTextures(1, &texture));
    }

  GL_CHECK(glGenTextures(1, &texture));
  GL_CHECK(glBindTexture(GL_TEXTURE_2D, texture));

  GL_CHECK(glTexParameteri(GL_TEXTURE_2D,
                           GL_TEXTURE_MIN_FILTER,
                           GL_LINEAR_MIPMAP_LINEAR));
  GL_CHECK(glTexParameteri(GL_TEXTURE_2D,
                           GL_TEXTURE_MAG_FILTER,
                           GL_LINEAR));
  GL_CHECK(glTexParameteri(GL_TEXTURE_2D,
                           GL_TEXTURE_WRAP_S,
                           GL_CLAMP_TO_EDGE));
  GL_CHECK(glTexParameteri(GL_TEXTURE_2D,
                           GL_TEXTURE_WRAP_T,
                           GL_CLAMP_TO_EDGE));

  GL_CHECK(glTexImage2D(GL_TEXTURE_2D,
                        0,
                        GL_RGB,
                        surface->w,
                        surface->h,
                        0,
                        GL_RGB,
                        GL_UNSIGNED_BYTE,
                        surface->pixels));

  GL_CHECK(glGenerateMipmap(GL_TEXTURE_2D));

  return 1;
}

void free_render()
{
  if (shader_program)
    {
      glDeleteProgram(shader_program);
      shader_program = 0;
    }

  load_texture(NULL);

  IMG_Quit();
}

int init_render()
{
  update_viewport();

  glClearColor(0.0, 0.0, 0.0, 1.0);

  view.x = view.y = 0.f;
  view.zoom = 1.f;
  view.rotate = 0.f;

  if (!use_shader())
    {
      free_render();
      return 0;
    }

  if (!IMG_Init(IMG_INIT_JPG | IMG_INIT_PNG | IMG_INIT_TIF | IMG_INIT_WEBP))
    {
      free_render();
      return 0;
    }

  GL_CHECK(glEnableVertexAttribArray(attrib_position));
  GL_CHECK(glEnableVertexAttribArray(attrib_texposition));
  GL_CHECK(glVertexAttribPointer(attrib_position,
                                 2,
                                 GL_FLOAT,
                                 GL_FALSE,
                                 0,
                                 vertex_array(1, 1)));

  GL_CHECK(glVertexAttribPointer(attrib_texposition,
                                 2,
                                 GL_FLOAT,
                                 GL_FALSE,
                                 0,
                                 tex_vertices));
  return 1;
}

static int render_nop() { return 1; }
static int (*render_fn)();

static int render_image()
{
  float m[9];
  make_view_matrix(m);
  GL_CHECK(glUniformMatrix3fv(uniform_view, 1, GL_FALSE, m));

  GL_CHECK(glClear(GL_COLOR_BUFFER_BIT));

  GL_CHECK(glDrawArrays(GL_TRIANGLE_STRIP, 0, 4));

  SDL_GL_SwapWindow(empic_window);

  return 1;
}

int render()
{
  int r = render_fn();
  render_fn = render_nop;

  return r;
}

void update_render()
{
  render_fn = render_image;
}

int load_image(const char * file)
{
  SDL_Surface * surface = IMG_Load(file);
  if (!surface)
    {
      SDL_Log("Couldn't load image: %s\n", file);
      return 0;
    }

  if (!load_texture(surface))
    return 0;

  vertex_array(surface->w, surface->h);

  SDL_FreeSurface(surface);

  return 1;
}

int zoom_cmd(char * cmd, char ** args)
{
  int err;
  float f;

  ASSERT_ARGS1(cmd, args);

  if (!strcmp(args[0], "in"))
    zoom_view_frac(1.1f);
  else if (!strcmp(args[0], "out"))
    zoom_view_frac(0.9f);
  else if (!strcmp(args[0], "fit-big"))
    zoom_view_fit(ZOOM_FIT_BIG);
  else if (!strcmp(args[0], "fit-small"))
    zoom_view_fit(ZOOM_FIT_SMALL);
  else if (!strcmp(args[0], "frac"))
    {
      ASSERT_ARGS(cmd, args + 1, "`frac' requires an argument");

      f = argtof(args[1], &err);
      if (err)
        {
          SDL_Log("%s: Can't convert '%s' to float", cmd, args[1]);
          return -1;
        }

      zoom_view_frac(f);
    }
  else
    {
      f = argtof(args[0], &err);

      if (err)
        {
          SDL_Log("%s: wrong argument: `%s', should be one of <float>, "
                  "`in', `out', `fit-big', `fit-small', `frac' <float>",
                  cmd, args[0]);
          return -1;
        }

      zoom_view(f);
    }

  update_render();
  return 0;
}

int rotate_cmd(char * cmd, char ** args)
{
  int err;
  float f;

  ASSERT_ARGS1(cmd, args);

  if (!strcmp(args[0], "top"))
    rotate_view(0.0f);
  else if (!strcmp(args[0], "left"))
    rotate_view(-90.0f);
  else if (!strcmp(args[0], "right"))
    rotate_view(90.0f);
  else if (!strcmp(args[0], "bottom"))
    rotate_view(180.0f);
  else if (!strcmp(args[0], "delta"))
    {
      ASSERT_ARGS(cmd, args + 1, "`delta' requires an argument");

      f = argtof(args[1], &err);
      if (err)
        {
          SDL_Log("%s: Can't convert '%s' to float", cmd, args[1]);
          return -1;
        }

      rotate_view_delta(f);
    }
  else
    {
      f = argtof(args[0], &err);

      if (err)
        {
          SDL_Log("%s: wrong argument: `%s', should be one of <float>, "
                  "`top', `left', `right', `bottom', `delta' <float>",
                  cmd, args[0]);
          return -1;
        }

      rotate_view(f);
    }

  update_render();
  return 0;
}

int move_cmd(char * cmd, char ** args)
{
  ASSERT_ARGS2(cmd, args);

  int ex, ey;
  float x, y;
  x = argtof(args[0], &ex);
  y = argtof(args[1], &ey);

  if (ex || ey)
    {
      SDL_Log("%s: Can't convert some arguments to float: %s, %s",
              cmd, args[0], args[1]);
      return -1;
    }

  move_view_delta(x, y);
  update_render();
  return 0;
}

void register_render_commands()
{
  register_command("zoom", zoom_cmd);
  register_command("rotate", rotate_cmd);
  register_command("move", move_cmd);
}
