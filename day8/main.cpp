#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>
using namespace std;

map<string, vector<string>> go;

map<string, map<char, int>> v;
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
    cout << cur << " what\n";
    if (fst == "")
      fst = cur;
    string L = line.substr(7, 3);
    string R = line.substr(12, 3);
    cout << L << " " << R << " XD\n";
    go[cur] = {L, R};
  }

  fst = "AAA";
  cerr << "first is " << fst << "\n";
  int id = 0;
  int n = s.length();
  while (fst != "ZZZ") {
    int sel = s[id % n] == 'R';
    fst = go[fst][sel];
    if (v[fst][sel]++ == 1) {
      cerr << "been here??\n";
    }
    id++;
    // cout << fst << " " << sel << "\n";
  }
  cout << id << "\n";
}
