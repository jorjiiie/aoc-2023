#include <iostream>
#include <vector>
using namespace std;

char grid[5000][5000];
int v[5000][5000];
int N = 140;
int dfs(int x, int y, int a, int b) {
  if (v[x][y])
    return 0;
  v[x][y] = 1;
  char c = grid[x][y];
  vector<int> pdx;
  switch (c) {
  case '|':
    pdx = {1, 0, -1, 0};
    break;
  case '-':
    pdx = {0, 1, 0, -1};
    break;
  case 'L':
    pdx = {-1, 0, 0, 1};
    break;
  case 'J':
    pdx = {-1, 0, 0, -1};
    break;
  case '7':
    pdx = {1, 0, 0, -1};
    break;
  case 'F':
    pdx = {1, 0, 0, 1};
    break;
  case 'S':
    exit(1);
  }
  if (a == x + pdx[0] && b == y + pdx[1]) {
    // we take the new one
    return 1 + dfs(x + pdx[2], y + pdx[3], x, y);
  } else {
    return 1 + dfs(x + pdx[0], y + pdx[1], x, y);
  }
}
int main() {
  string s;
  int idx = 0;
  int sx, sy;
  freopen("in.txt", "r", stdin);
  while (getline(cin, s)) {
    for (int i = 0; i < s.length(); i++) {
      grid[idx][i] = s[i];
      if (grid[idx][i] == 'S') {
        sx = idx;
        sy = i;
      }
    }
    idx++;
  }
  v[sx][sy] = 1;
  // figure out where s is!
  cout << "part 1 " << dfs(sx + 1, sy, sx, sy) << "\n";

  grid[sx][sy] = 'J';

  bool on = false;
  int cnt = 0;
  for (int i = 0; i < 140; i++) {
    on = false;
    int dir = -1;
    for (int j = 0; j < 140; j++) {
      if (v[i][j]) {
        switch (grid[i][j]) {
        case '|':
          on ^= 1;
          break;
        case '-':
          on = on;
          break;
        case 'L':
          dir = 0;
          break;
        case 'F':
          dir = 1;
          break;
        case 'J':
          // this changes based off of what we have seem before
          on = (dir == 0) ? on : (on ^ 1);
          break;
        case '7':
          on = (dir == 1) ? on : (on ^ 1);
          break;
        }
        continue;
      } else
        cnt += on;
    }
  }
  cout << "part 2: " << cnt << "\n";
}
