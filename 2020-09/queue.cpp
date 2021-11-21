/*
 * Inspired by the functional data structures world, a C++ queue class that
 * uses a pair of vectors.
 */

#include <algorithm>
#include <iostream>
#include <optional>
#include <vector>

template <typename T>
class Queue {
    //The last element in head is the first element out.
    std::vector<T> head;
    //The last element in tail is the last element in.
    std::vector<T> tail;

public:
    void enqueue(const T& value)
    {
        tail.push_back(value);
    }

    std::optional<T> dequeue()
    {
        if (head.empty()) {
            if (tail.empty()) {
                return std::nullopt;
            }
            head.swap(tail);
            std::reverse(head.begin(), head.end());
        }
        T value { head.back() };
        head.pop_back();
        return value;
    }

    void dump() const {
#if 0
        std::cout << "head: ";
        for (const auto& element: head) {
            std::cout << element << ", ";
        }
        std::cout << "\ntail: ";
        for (const auto& element: tail) {
            std::cout << element << ", ";
        }
        std::cout << '\n';
#endif
    }
};

int main()
{
    Queue<int> q;
    for (int i{0}; i < 5; ++i) {
        q.enqueue(i);
        q.dump();
    }
    for (int i{0}; i < 3; ++i) {
        std::cout << *(q.dequeue()) << '\n';
        q.dump();
    }
    for (int i{5}; i < 10; ++i) {
        q.enqueue(i);
        q.dump();
    }
    while (true) {
        auto element { q.dequeue() };
        if (not element) break;
        std::cout << *(element) << '\n';
        q.dump();
    }
}
