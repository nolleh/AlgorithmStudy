#include <string>
#include <vector>
#include <ostream>
#include <iostream>

int game_board2(std::vector<std::string> maps, int col_size, int i);
int game_board3(std::vector<std::string> maps, int col_size, int i, int e);
bool check_all(const std::vector<std::string>& map);

std::ostream& operator<<(std::ostream& os, const std::vector<std::string>& vec_string);

auto make_test_pairs()
{
  std::vector<std::pair<std::pair<int, int>, std::vector<std::string>>> pairs {
     { {3, 7},  {"#.....#", "#.....#", "##...##"}},
     { {3, 7},  {"#.....#", "#.....#", "##..###"}}};
     // { {8, 10}, {"##########", "#........#", "#........#", "#........#", "#........#", "#........#", "#........#", "##########"}}};

  return pairs;
}

int game_board(std::vector<std::string> maps, int col_size)
{
  return game_board2(maps, col_size, 0);
}

// 현재 i 에 대해 해당 값을 선택 / 비선택
int game_board2(std::vector<std::string> maps, int col_size, int i)
{
  if (maps.size() * col_size - 1 == i) 
  {
    std::cout << maps << std::endl;
    if (check_all(maps))  return 1;
    else return 0;
  }

  int r = i / col_size;
  int c = i % col_size;

  if (maps[r][c] != '.')
  {
    return game_board2(maps, col_size, i+1);
  }
  else
  {
    int pick = game_board3(maps, col_size, i, 0) + game_board3(maps, col_size, i, 1) + 
               game_board3(maps, col_size, i, 2) + game_board3(maps, col_size, i, 3);
    int unpick = game_board2(maps, col_size, i+1);

    return pick + unpick;
  }
}

// 개별 select 에 대해 recurse 로 나머지 처리 시작
int game_board3(std::vector<std::string> maps, int col_size, int i, int e)
{
  int r = i / col_size;
  int c = i % col_size;
  
  if (maps[r][c] != '.') return 0;

  maps[r][c] = std::to_string(e)[0];
  
  switch (e)
  {
    // ㄱ
    case 0:
      if (maps[r][c+1] != '.' || maps[r+1][c+1] != '.') return 0;
      maps[r][c+1] = '0'; maps[r+1][c+1] = '0';
    // ㄴ
    case 1:
      if (maps[r+1][c] != '.' || maps[r+1][c+1] != '.') return 0;
      maps[r+1][c] = '1'; maps[r+1][c+1] = '1';
      break;
    // r
    case 2:
      if (maps[r][c+1] != '.' || maps[r+1][c] != '.') return 0;
      maps[r][c+1] = '2'; maps[r+1][c] = '2';
      break;
    // _|
    case 3:
      if (maps[r+1][c-1] != '.' || maps[r+1][c] != '.') return 0;
      maps[r+1][c-1] = '3'; maps[r+1][c] = '3';
      break;
    default:
      std::cout << "enum is not allowed to " << e << std::endl;
  }

  return game_board2(maps, col_size, i+1);
}

bool check_all(const std::vector<std::string>& map)
{
  return std::all_of(begin(map), end(map), [](const std::string& m){
    return std::all_of(begin(m), end(m), [](const auto& c){
      return c != '.';
    });
  });
}

std::ostream& operator<<(std::ostream& os, const std::pair<int,int>& pair)
{
  return os << "(" << pair.first << "," << pair.second << ")";
}

std::ostream& operator<<(std::ostream& os, const std::vector<std::string>& vec_string)
{
  // os << "[";
  os << std::endl;
  for (auto& str : vec_string)
  {
    os << str << std::endl;
  }
  // os << "]";
  return os;
}

int main() 
{
  for (auto test: make_test_pairs())
  {
    std::pair<int, int> array_info;
    std::vector<std::string> boards;
    std::tie(array_info, boards) = test;

    std::cout << "input:" << array_info << ", col_size:" << array_info.second << "," << boards << std::endl;
    std::cout << "output:" << game_board(boards, array_info.second) << std::endl;
  }
  return 0;
}