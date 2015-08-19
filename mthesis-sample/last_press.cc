#include "last_press.h"
#include <unistd.h>
#include <sys/select.h>
#include <termios.h>

cfrp::event last_press_input_node::process()
{
  struct termios saved, t;
  tcgetattr(STDIN_FILENO, &saved);
  t = saved;
  t.c_lflag &= ~(ICANON | ECHO);
  tcsetattr(STDIN_FILENO, TCSANOW, &t);

  fd_set fds;
  FD_ZERO(&fds);
  FD_SET(STDIN_FILENO, &fds);
  timeval to;
  to.tv_sec = to.tv_usec = 0;
  const int status = select(1, &fds, NULL, NULL, &to);
  const bool changed = status > 0;

  if (changed) {
    read(STDIN_FILENO, &last_, 1);
  }

  tcsetattr(STDIN_FILENO, TCSANOW, &saved);
  return cfrp::event(changed, cfrp::value((int)last_));
}
