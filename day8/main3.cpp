#include <cstring>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>
using namespace std;

int go[1000][2];
int cstate = 0;
int states[6];

map<string, int> ids;
map<int, string> rev;
int GLOBAL = 0;

int end_[1000] = {};

int vis[1000][1000] = {};
int n;
string s;

int LEN[6];
int tm = 0;
int v = 0;
int xy = 0;
void dfs(int id) {
  int sel = s[tm++ % n] == 'R';
  if (end_[id]) {
    v++;
    cout << tm - 1 << " JAJAJAJJAJAJAJ\n";
    cout << rev[id] << " jaj\n";
  }
  if (v > 1) {
    LEN[xy] = (tm - 1) - vis[id][(tm - 1) % n];
    cout << (vis[id][(tm - 1) % n]) << " " << tm - 1 << "\n";
    cout << "what\n";
    return;
  }
  vis[id][(tm - 1) % n] = tm - 1;
  dfs(go[id][sel]);
}

int main() {
  cin >> s;

  string zz;
  getline(cin, zz);
  getline(cin, zz);
  string line = "";
  string fst = "";
  while (getline(cin, line)) {
    string cur = line.substr(0, 3);

    string L = line.substr(7, 3);
    string R = line.substr(12, 3);

    if (ids.find(cur) == ids.end())
      ids[cur] = GLOBAL++, rev[GLOBAL - 1] = cur;
    if (ids.find(L) == ids.end())
      ids[L] = GLOBAL++, rev[GLOBAL - 1] = L;
    if (ids.find(R) == ids.end())
      ids[R] = GLOBAL++, rev[GLOBAL - 1] = R;

    go[ids[cur]][0] = ids[L];
    go[ids[cur]][1] = ids[R];

    if (cur.back() == 'A') {
      cout << cur << "\n";
      states[cstate++] = ids[cur];
    }
    if (cur.back() == 'Z') {
      cout << cur << " HI \n";
      end_[ids[cur]] = 1;
    }
  }

  for (int i = 0; i < 6; i++) {
    tm = 0;
    v = 0;
    n = s.length();
    memset(vis, 0, sizeof(vis));
    dfs(states[i]);
    xy++;
  }
  for (int i = 0; i < 6; i++) {
    cout << "CYCLE LEN IS " << LEN[i] << "\n";
  }
  for (int i = 0; i < 6; i++) {
    cout << LEN[i] << " ";
  }
  cout << "\n";
  cout << states[1] << "\n";
  cout << s << "\n";
}
