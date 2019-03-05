#include <thread>
#include <iostream>
#include <omp.h>

// g++ -fopenmp -I/usr/include test_omp.cpp

int main()
{
    using namespace std::chrono_literals;
#pragma omp parallel for num_threads(4)
    for (int i = 0; i < 20; ++i)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        std::cout << i << "th loop in thread " << omp_get_thread_num() << std::endl;
    }

    std::cout << std::endl;
    return 0;
}
