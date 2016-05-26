#define _GNU_SOURCE
#define DEV_INPUT_EVENT "/dev/input"
#define EVENT_DEV_NAME "event"
#define DEV_FB "/dev"
#define FB_DEV_NAME "fb"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <linux/fb.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <time.h>
#include <poll.h>
#include <dirent.h>
#include <string.h>
/* #include <unistd.h> */

#include <linux/input.h>
#include <linux/fb.h>

#include <chibi/eval.h>

#define SENSE_HAT_FB_FBIOGET_GAMMA 61696
#define SENSE_HAT_FB_FBIOSET_GAMMA 61697
#define SENSE_HAT_FB_FBIORESET_GAMMA 61698
#define SENSE_HAT_FB_GAMMA_DEFAULT 0
#define SENSE_HAT_FB_GAMMA_LOW 1
#define SENSE_HAT_FB_GAMMA_USER 2

struct fb_t {
  uint16_t pixel[64];
};

static int is_event_device(const struct dirent *dir)
{
  return strncmp(EVENT_DEV_NAME, dir->d_name,
                 strlen(EVENT_DEV_NAME)-1) == 0;
}
static int is_framebuffer_device(const struct dirent *dir)
{
  return strncmp(FB_DEV_NAME, dir->d_name,
                 strlen(FB_DEV_NAME)-1) == 0;
}

static int open_evdev(const char *dev_name)
{
  struct dirent **namelist;
  int i, ndev;
  int fd = -1;

  ndev = scandir(DEV_INPUT_EVENT, &namelist, is_event_device, versionsort);
  if (ndev <= 0)
    return ndev;

  for (i = 0; i < ndev; i++)
    {
      char fname[64];
      char name[256];

      snprintf(fname, sizeof(fname),
               "%s/%s", DEV_INPUT_EVENT, namelist[i]->d_name);
      fd = open(fname, O_RDONLY);
      if (fd < 0)
        continue;
      ioctl(fd, EVIOCGNAME(sizeof(name)), name);
      if (strcmp(dev_name, name) == 0)
        break;
      close(fd);
    }

  for (i = 0; i < ndev; i++)
    free(namelist[i]);

  return fd;
}

static int test_bit(const char* bitmask, int bit) {
  int i = bit >> 3;
  char mask = 1 << (bit & 0x07);
  return bitmask[i] & mask;
}

sexp evdev_keydownp_scm(sexp ctx, sexp self, sexp n,
                        sexp fd_obj, sexp key_obj) {
  int max = KEY_MAX,
    fd = sexp_unbox_fixnum(fd_obj),
    key = sexp_unbox_fixnum(key_obj),
    evtype = EV_KEY,
    ret;

  char bytes[(max+7) << 8];
  memset(bytes, 0, sizeof bytes);
    
  ret = ioctl(fd, EVIOCGKEY(sizeof(bytes)), &bytes);

  if (test_bit(bytes, key))
    return SEXP_TRUE;
  else
    return SEXP_FALSE;
}


sexp clock_gettime_scm(sexp ctx, sexp self, sexp n, sexp clockid_obj) {
  unsigned int usec = sexp_unbox_fixnum(clockid_obj);
  sexp_gc_var3(sec, nsec, timevec);
  sexp_gc_preserve3(ctx, sec, nsec, timevec);
  timevec = sexp_make_vector(ctx, SEXP_TWO, SEXP_FALSE);
  struct timespec ts;
  int res;

  res = clock_gettime(sexp_unbox_fixnum(clockid_obj), &ts);
  sec = sexp_make_integer(ctx, ts.tv_sec);
  nsec = sexp_make_integer(ctx, ts.tv_nsec);
  sexp_vector_set(timevec, SEXP_ZERO, sec);
  sexp_vector_set(timevec, SEXP_ONE, nsec);
  sexp_gc_release3(ctx);
  return timevec;
}

sexp usleep_scm(sexp ctx, sexp self, sexp n, sexp usec_obj) {
  unsigned int usec = sexp_unbox_fixnum(usec_obj);
  usleep(usec);
  return SEXP_TRUE;
}

sexp fb_lowlight_scm(sexp ctx, sexp self, sexp n,
                     sexp fb_obj, sexp lowp_obj) {

  int fd = sexp_unbox_fixnum(sexp_car(fb_obj)),
    lowp = sexp_unbox_boolean(lowp_obj),
    ret;
  ret = ioctl(fd, SENSE_HAT_FB_FBIORESET_GAMMA,
              lowp ? SENSE_HAT_FB_GAMMA_LOW :
              SENSE_HAT_FB_GAMMA_DEFAULT); 
  return sexp_make_boolean(ret != -1);
}

sexp evdev_keys_scm(sexp ctx, sexp self, sexp n, sexp fd_obj) {
  int max = KEY_MAX,
    fd = sexp_unbox_fixnum(fd_obj),
    evtype = EV_KEY,
    ret;

  char bytes[(max+7)/8];
  memset(bytes, 0, sizeof bytes);
    
  ret = ioctl(fd, EVIOCGKEY(sizeof(bytes)), &bytes);

  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);

  res = SEXP_NULL;
  
  int i;
  for (i=0; i < max; i++) {
    if (test_bit(bytes, i)) {
      res = sexp_cons(ctx, sexp_make_integer(ctx, i), res);
    }
  }

  sexp_gc_release1(ctx);
  
  return res; 
}

sexp read_evdev_scm(sexp ctx, sexp self, sexp n, sexp fd_obj) {
  struct input_event ev;
  struct pollfd evpoll = { .events = POLLIN };
  int fd = sexp_unbox_fixnum(fd_obj);
  int i, rd;

  evpoll.fd = fd;
  if(poll(&evpoll, 1, 0) == 0) {
    return SEXP_FALSE;
  }
  rd = read(fd, &ev, sizeof(struct input_event));
  if (rd < 0 || ev.type != EV_KEY || (ev.value != 1 && ev.value != 2)) {
    return SEXP_FALSE;
  }
  return sexp_make_integer(ctx, ev.code);
}

sexp close_evdev_scm(sexp ctx, sexp self, sexp n, sexp fd_obj) {
  int fd = sexp_unbox_fixnum(fd_obj);
  return sexp_make_integer(ctx, close(fd));
}

static int open_fbdev(const char *dev_name)
{
  struct dirent **namelist;
  int i, ndev;
  int fd = -1;
  struct fb_fix_screeninfo fix_info;

  ndev = scandir(DEV_FB, &namelist, is_framebuffer_device, versionsort);
  if (ndev <= 0)
    return ndev;

  for (i = 0; i < ndev; i++)
    {
      char fname[64];
      char name[256];

      snprintf(fname, sizeof(fname),
               "%s/%s", DEV_FB, namelist[i]->d_name);
      fd = open(fname, O_RDWR);
      if (fd < 0)
        continue;
      ioctl(fd, FBIOGET_FSCREENINFO, &fix_info);
      if (strcmp(dev_name, fix_info.id) == 0)
        break;
      close(fd);
      fd = -1;
    }
  for (i = 0; i < ndev; i++)
    free(namelist[i]);

  return fd;
}

sexp open_evdev_scm(sexp ctx, sexp self, sexp n, sexp dev_name_sexp) {
  if (sexp_stringp(dev_name_sexp)) {
    char *dev_name= strndup(sexp_string_data(dev_name_sexp),
                            sexp_string_length(dev_name_sexp));
    int ret = open_evdev(dev_name);
    free(dev_name);
    return sexp_make_integer(ctx, ret);
  } else {
    return SEXP_NEG_ONE;
  }
}

sexp open_fb_scm(sexp ctx, sexp self, sexp n, sexp dev_name_sexp) {
  int fd = -1;
  struct fb_t *fb = NULL;
  
  if (sexp_stringp(dev_name_sexp)) {
    char *dev_name= strndup(sexp_string_data(dev_name_sexp),
                            sexp_string_length(dev_name_sexp));
    fd = open_fbdev(dev_name);
    free(dev_name);
  }  
  
  if (fd > 0) {
    fb = mmap(0, 128, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (!fb) {
      close(fd);
      fd = -1;
    } else {
      memset(fb, 0, 128);
    }
  }
  sexp_gc_var4(fd_obj, fb_obj, fb_length_obj, res);
  sexp_gc_preserve4(ctx, fd_obj, fb_obj, fb_length_obj, res);

  fd_obj = sexp_make_integer(ctx, fd);

  if (fb) {
    fb_length_obj = sexp_make_integer(ctx, sizeof(fb));
    fb_obj = sexp_make_bytes(ctx, fb_length_obj, 0);
    char *fb_obj_data = sexp_bytes_data(fb_obj);
    memcpy(fb_obj_data, &fb, sizeof(fb));
  } else {
    fb_obj = SEXP_FALSE;
  }

  res = sexp_list2(ctx, fd_obj, fb_obj);
  sexp_gc_release4(ctx);
  return res;
}

sexp close_fb_scm(sexp ctx, sexp self, sexp n, sexp fb_pair) {
  sexp res = SEXP_FALSE;
  if (sexp_pairp(fb_pair)) {
      sexp fd_obj = sexp_car(fb_pair), fb_obj = sexp_car(sexp_cdr(fb_pair));
      if (sexp_integerp(fd_obj) && sexp_bytesp(fb_obj)) {
        int fb_length = sexp_bytes_length(fb_obj);
        if (fb_length == sizeof(struct fb_t *)) {
          int fd = sexp_unbox_fixnum(fd_obj);
          struct fb_t **fb = sexp_bytes_data(fb_obj);
          close(fd);
          munmap(*fb, sizeof(struct fb_t));
          res = SEXP_TRUE;
        }
      }
  }
  return res;
}

const int xo = 0, yo = 0, wf = 8, hf = 8;

sexp blit_fb_scm(sexp ctx, sexp self, sexp n,
                 sexp fb_obj, sexp bm_obj, sexp width_obj, sexp height_obj,
                 sexp x_obj, sexp y_obj) {
  
  int w = sexp_unbox_fixnum(width_obj),
    h = sexp_unbox_fixnum(height_obj),
    x = sexp_unbox_fixnum(x_obj),
    y = sexp_unbox_fixnum(y_obj);
  struct fb_t **fb = sexp_bytes_data(fb_obj);
  uint16_t *bm = sexp_bytes_data(bm_obj);

  /* printf("whxy: %d %d %d %d\n", w, h, x, y); */
  
  int y1, x1;
  for (y1 = 0; y1 < h; y1++) {
    int fby = y1 + yo - y;

    if (fby < 0) continue;
    if (fby >= wf) break;
    
    for (x1 = 0; x1 < w; x1++) {
      int fbx = x1 + xo - x;

      if (fbx < 0) continue;
      if (fbx >= hf) break;

      int i = (fby * wf + fbx);
      int j = (y1 * w + x1);

      (*fb)->pixel[i] = bm[j];
    }
  }
  return SEXP_TRUE;
}

int main(int argc, char* args[])
{
  sexp ctx, env;
  ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 0);
  env = sexp_context_env(ctx);
  sexp_define_foreign(ctx, env, "usleep", 1, usleep_scm);
  sexp_define_foreign(ctx, env, "clock-gettime", 1, clock_gettime_scm);
  sexp_define_foreign(ctx, env, "evdev-open", 1, open_evdev_scm);
  sexp_define_foreign(ctx, env, "evdev-close", 1, close_evdev_scm);  
  sexp_define_foreign(ctx, env, "evdev-read", 1, read_evdev_scm);
  sexp_define_foreign(ctx, env, "evdev-keys", 1, evdev_keys_scm);
  sexp_define_foreign(ctx, env, "evdev-keydown?", 2, evdev_keydownp_scm);
  sexp_define_foreign(ctx, env, "framebuffer-open", 1, open_fb_scm);
  sexp_define_foreign(ctx, env, "framebuffer-close", 1, close_fb_scm);
  sexp_define_foreign(ctx, env, "framebuffer-blit-primitive", 6, blit_fb_scm);
  sexp_define_foreign(ctx, env, "framebuffer-low-light", 2, fb_lowlight_scm);

  sexp_gc_var1(prelude_filename_obj);
  sexp_gc_preserve1(ctx, prelude_filename_obj);

  prelude_filename_obj = sexp_c_string(ctx, "./prelude.scm", -1);
  sexp_load(ctx, prelude_filename_obj, NULL);
  	
  sexp_gc_release1(ctx);
  sexp_destroy_context(ctx);

  return 0;
}
