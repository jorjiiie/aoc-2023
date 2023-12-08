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
int GLOBAL = 0;

int end_[1000] = {};

int main() {
  string s;
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
      ids[cur] = GLOBAL++;
    if (ids.find(L) == ids.end())
      ids[L] = GLOBAL++;
    if (ids.find(R) == ids.end())
      ids[R] = GLOBAL++;

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

  // we really only care about edges from a -> z
  // do we really just draw lines from a -> z and then z -> z??

  long long id = 0;
  int n = s.length();
  long long k = 1;

  int m = ids.size();

  while (true) {
    bool bad = false;
    for (int i = 0; i < 6; i++) {
      if (!end_[states[i]]) {
        bad = true;
        break;
      }
    }
    if (!bad)
      break;
    int sel = s[id % n] == 'R';
    id++;
    for (int i = 0; i < 6; i++) {
      states[i] = go[states[i]][sel];
    }
    if (id == k) {
      cout << id << "\n";
      k *= 2;
    }
  }
  cout << id << "\n";
}
