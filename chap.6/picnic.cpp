#include <iostream>
#include <vector>
#include <algorithm>

auto make_test_pairs()
{
  std::vector<std::pair<std::vector<std::pair<int, int>>, int>> pairs {
  	{ {{0,1}}, 2 },
    { {{0,1}, {1,2}, {2,3}, {3,0}, {0,2}, {1,3}}, 4 },
    { {{0,1}, {0,2}, {1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}, {3,5}, {4,5}}, 6}  };

  return pairs;
}

int count(std::vector<std::pair<int, int>> pairs, int size, std::vector<int> picks)
{
  if (pairs.empty())
  {
  	if (picks.size() == size)
  		return 1;
  	else
  		return 0;
  }
  
  using value_type = decltype(pairs);

  auto head = begin(pairs);
  auto tail = head + 1;

  value_type pick_rest, unpick_rest;
  std::vector<int> new_picks;
  
  std::copy_if(
  	  tail, end(pairs), std::back_inserter(pick_rest), 
  	  [&](auto& pair)
  	  {
  	  	return ((pair.first != head->first) && (pair.second != head->first) &&
  	           (pair.first != head->second) && (pair.second != head->second));
  	  });

  std::copy(tail, end(pairs), std::back_inserter(unpick_rest));

  std::copy(begin(picks), end(picks), std::back_inserter(new_picks));
  new_picks.push_back(head->first);
  new_picks.push_back(head->second);

  return count(pick_rest, size, new_picks) + count(unpick_rest, size, picks);
}

int main()
{
  for (auto& test: make_test_pairs())
  {
  	decltype(test.first) friends;
  	decltype(test.second) size;
  	std::tie(friends, size) = test;
  	for (auto& pair: friends)
  	{
  	  std::cout << "(" << pair.first << "," << pair.second << ") ";
    }
    std::cout << ": " << count(friends, size, {}) << std::endl;
  }

  return 0;
}