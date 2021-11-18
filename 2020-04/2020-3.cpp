#include <algorithm>
#include <iostream>
#include <string>
//#include <string_view>
#include <vector>

using namespace std::literals;
using string_view = const std::string&;

int main()
{
    std::vector<std::string> passports;
    std::string passport;

    auto push_passport = [&]()
    {
        if (not passport.empty()) {
            passports.push_back(passport);
            passport.clear();
        }
    };

    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) {
            push_passport();
        } else {
            passport += ' ';
            passport += line;
        }
    }
    push_passport();

    auto is_valid = [](string_view passport)
    {
        const std::vector<std::string> required {
            "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", //"cid",
        };

        auto has_field = [passport](string_view field)
        {
            return passport.npos != passport.find(" "s + field + ':');
        };

        return std::all_of(required.begin(), required.end(), has_field);
    };

    std::cout
        << std::count_if(passports.begin(), passports.end(), is_valid) << '\n';
}
