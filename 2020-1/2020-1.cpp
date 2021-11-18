/*
 * Given a list of integers less than 10,000 from stdin,
 * find the two that add to 2020,
 * output the product of the two numbers.
 */

//TODO: We do the search twice. We should only have to do it once.

#include <algorithm>
#include <iostream>
#include <vector>

int main()
{
    //This vector will hold the values of (2020 - n) for values we've seen.
    //Due to being more cache-friendly, binary searches of a sorted vector is
    //likely to be faster than a set or unordered_set despite overhead of
    //inserting into the vector.
    std::vector<int> seen;
    int n;
    while (std::cin >> n) {
        //If this value is in seen, we've found our answer.
        if (std::binary_search(seen.begin(), seen.end(), n)) {
            std::cout << (n * (2020 - n)) << '\n';
            break;
        }
        //We haven't found the answer...
        //Insert (2020 - n) into seen keeping it sorted.
        n = 2020 - n;
        auto place { std::lower_bound(seen.begin(), seen.end(), n) };
        seen.insert(place, n);
    }
}
