#include <algorithm>
#include <iostream>
#include <unordered_set>

int main()
{
    std::unordered_set<int> seen;
    int n;
    while (std::cin >> n) {
        //If this value is in seen, we've found our answer.
        if (seen.count(n) > 0) {
            std::cout << (n * (2020 - n)) << '\n';
            break;
        }
        //We haven't found the answer...
        //Insert (2020 - n) into seen.
        seen.insert(2020 - n);
    }
}
