/*
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
*/

#include <algorithm>
#include <functional>
#include <iomanip>
#include <iostream>
#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std::literals;
using cstrref = const std::string&;
using field = std::pair<std::string, std::string>;
using parsed_passport = std::vector<field>;

const std::regex hcl_rx { "#[0-9a-f]{6}" };
const std::regex pid_rx { "[0-9]{9}" };

bool check_range(int number, int min, int max)
{
    return (number >= min) and (number <= max);
}

bool valid_ecl(cstrref s)
{
    const std::vector<std::string> choices{
        "amb", "blu", "brn", "gry", "grn", "hzl", "oth"
    };
    return choices.end() != std::find(choices.begin(), choices.end(), s);
}

int stoi(cstrref s, std::size_t* pos = nullptr)
{
    //We always do range validation after stoi,
    //so returning zero on error works.
    try {
        return std::stoi(s, pos);
    } catch (...) {
        return 0;
    }
}

bool valid_hgt(cstrref s)
{
    std::size_t pos{0};
    int n { ::stoi(s, &pos) };
    auto units { s.substr(pos) };
    if (units == "cm") return check_range(n, 150, 193);
    if (units == "in") return check_range(n, 59, 76);
    return false;
}

std::unordered_map<std::string, std::function<bool(cstrref)>> validations{
    { "byr", [](cstrref s){ return check_range(::stoi(s), 1920, 2002); }},
    { "iyr", [](cstrref s){ return check_range(::stoi(s), 2010, 2020); }},
    { "eyr", [](cstrref s){ return check_range(::stoi(s), 2020, 2030); }},
    { "hgt", valid_hgt },
    { "hcl", [](cstrref s){ return std::regex_match(s, hcl_rx); }},
    { "ecl", valid_ecl },
    { "pid", [](cstrref s){ return std::regex_match(s, pid_rx); }},
};

bool valid_field(const field& f)
{
    auto iter { validations.find(f.first) };
    if (validations.end() == iter) return true;
    auto ok { iter->second(f.second) };
#if 0
    if (not ok) {
        std::cout << __func__ << '(' << std::quoted(f.first) << ", " << std::quoted(f.second) << ")\n";
    }
#endif
    return ok;
}

parsed_passport parse(cstrref passport)
{
    std::istringstream stream{passport};
    parsed_passport parsed;
    std::string part;
    while (std::getline(stream, part, ' ')) {
        if (part.empty()) continue;
        auto colon { part.find(':') };
        parsed.push_back(
                std::make_pair(
                    part.substr(0, colon),
                    part.substr(colon + 1)));
    }
    return parsed;
}

bool valid_fields(cstrref passport)
{
#if 0
    std::cout << __func__ << '(' << std::quoted(passport) << ")\n";
#endif
    auto parsed { parse(passport) };
    return std::all_of(parsed.begin(), parsed.end(), valid_field);
}

//This could be combined with field validations...
//...since we only validate required fields.
//But this way is more flexible.
bool valid_required_fields(cstrref passport)
{
    const std::vector<std::string> required {
        "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", //"cid",
    };

    auto has_field = [passport](cstrref field)
    {
        return passport.npos != passport.find(" "s + field + ':');
    };

    return std::all_of(required.begin(), required.end(), has_field);
}

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

    auto is_valid = [](cstrref passport)
    {
        return valid_required_fields(passport) and valid_fields(passport);
    };

    std::cout
        << std::count_if(passports.begin(), passports.end(), is_valid) << '\n';
}
