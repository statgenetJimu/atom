#include <algorithm>
#include <chrono>
#include <iostream>
#include <iterator>
#include <random>
#include <vector>

/* Compile with :
   g++ -std=c++11 -march=native -O3 -o test_sequential parallel.cpp
   g++ -std=c++11 -fopenmp -D_GLIBCXX_PARALLEL -march=native -O3 -o test_parallel parallel.cpp

   c++11 is required as a minimum standard since c++11 features are used
   in order for the libc (standard library) to be implemented with parallel functions we need the
   following define : _GLIBCXX_PARALLEL plus the openMP flag. (This will make std::sort parallel)
   the architecture flag is used in order for the parallel implementation to use architecture
   specific atomic/mutex operations.
*/

#define __TEST_VECTOR_LENGTH__ 100000000
#define __RANDOM_SEED__ 0

int main() {

    std::vector<unsigned int> test_vector(__TEST_VECTOR_LENGTH__, 0);
    std::iota(test_vector.begin(), test_vector.end(), 0); /* Assign with 0,1,2,3,... */

    std::srand(__RANDOM_SEED__); /* This needs to be the fixed in order to compare the runs */
    std::random_shuffle(test_vector.begin(), test_vector.end()); /* Shuffle the values */

    /* Init time */
    std::chrono::time_point<std::chrono::system_clock> start, end;
    start = std::chrono::system_clock::now();

    /* Sorting */
    std::sort(test_vector.begin(), test_vector.end());

    /* Final time */
    end = std::chrono::system_clock::now();
    std::chrono::duration<double> elapsed_seconds = end-start;
    std::cout << "Elapsed time : " << elapsed_seconds.count() << " [s]" << std::endl;

    return 0;
}
