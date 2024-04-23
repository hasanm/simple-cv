#include "MyContainer.h"
using namespace std;
using namespace cv; 


MyContainer::MyContainer() {
    
}

MyContainer::~MyContainer() {
}

int MyContainer::load_image(char *filename, int color_mode) {
    Mat mat = imread(filename, color_mode);
    store.push_back(mat);
    size_t pos = store.size() - 1;
    return pos;
}

int MyContainer::new_lines() {
    lines.clear();
    return 0; 
}

int MyContainer::push_line(int x1, int x2, int y1, int y2) {
    Vec4i v(x1, x2, y1, y2);
    lines.push_back(v);
    return 0; 
}

vector<Vec4i> MyContainer::get_lines() {
    return lines;
} 
