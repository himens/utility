#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>
#include <chrono>

#ifndef TIMER_H
#define TIMER_H

#define sleep_ms(x) ( std::this_thread::sleep_for(std::chrono::milliseconds(x)) )
#define sleep_s(x) ( std::this_thread::sleep_for(std::chrono::seconds(x)) )

/////////////////
// Timer class //
/////////////////
class Timer
{
  public:
    Timer(const std::string name) { set_name(name); }
    Timer() {};

    void start() { _start = std::chrono::system_clock::now(); }
    void stop() { _end = std::chrono::system_clock::now(); }
    void reset() { _start = _end = {}; }
    void set_name(const std::string name) { _name = name; }
    std::string get_name() const { return _name; }
    double get_elapsed_time_us() const { return get_elapsed_time_s() * 1e6; }
    double get_elapsed_time_ms() const { return get_elapsed_time_s() * 1e3; }
    double get_elapsed_time_s() const
    { 
      std::chrono::duration<double> elapsed_seconds = _end - _start;

      return elapsed_seconds.count();
    }

  private:  
    std::string _name;
    std::chrono::time_point<std::chrono::system_clock> _start;
    std::chrono::time_point<std::chrono::system_clock> _end;
};
#endif

