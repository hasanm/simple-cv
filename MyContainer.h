#ifndef MY_CONTAINER_H
#define MY_CONTAINER_H

#include<vector>
#include <opencv2/opencv.hpp>

using namespace cv;
using namespace std;

class MyContainer {
 public:
    MyContainer();
    ~MyContainer();

    int load_image(char* filename, int color_mode);
    int new_lines();
    int push_line(int x1, int x2, int y1, int y2);
    vector<Vec4i> get_lines();
 private:
    vector<Mat> store;
    vector<Vec4i> lines;
    int current_index; 
}; 

#endif
