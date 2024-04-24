// C program to display hostname
// and IP address
#include <opencv2/opencv.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgproc.hpp>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <fstream> 
#include <map>
#include <vector> 
#include "simple.h"
#include "MyContainer.h"

using namespace cv;
using namespace std;

static const char INPUT_STRING[] = "/data/aoe_images/my.png";
static const char OUTPUT_STRING[] = "/data/aoe_images/out.png";
static const char LINES_FILENAME[] = "/data/aoe_images/lines.txt";
static const char RHOS_FILENAME[] = "/data/aoe_images/rhos.txt";

int copy_image(char *filename)
{
    Mat mat;
    try {
    mat = imread(filename, IMREAD_COLOR);
    Mat dst = mat.clone();
    imwrite(INPUT_STRING, dst);
    imwrite(OUTPUT_STRING, dst);
    return 0;
    } catch (cv::Exception e) {
        cout << "Cuaght Exception" <<endl;
        return -1;
    }
}


int paste_image(char *filename)
{
    Mat mat;
    try {
        mat = imread(OUTPUT_STRING, IMREAD_COLOR);
        Mat dst = mat.clone();
        imwrite(filename, dst);
        return 0;
    } catch (cv::Exception e) {
        cout << "Cuaght Exception" <<endl;
        return -1;
    }
}


int cut_image(int x, int y, int p, int q)
{
    Mat mat;
    try {
    mat = imread("/data/aoe_images/my.png", IMREAD_COLOR);
    Mat dst = mat(Rect(Point(x,y), Point(p,q)));
    imwrite("/data/aoe_images/out.png", dst);
    return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        return -1;
    }
}



int  draw_rectangle(int x, int y, int p, int q)
{
    Mat mat;
    try {
    mat = imread(INPUT_STRING, IMREAD_COLOR);
    Mat dst = mat.clone();
    rectangle (dst, Point(x,y), Point (p,q), Scalar(0), 2);
    imwrite(OUTPUT_STRING, dst);
    return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        return -1;
    }
}


int make_gray()
{
    Mat mat;
    Mat dst;
    try {
        mat = imread(INPUT_STRING, IMREAD_COLOR);
        cvtColor(mat, dst, COLOR_BGR2GRAY);
        imwrite(OUTPUT_STRING, dst);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        return -1;
    }
}


int my_merge ()
{
    Mat src;
    Mat dst;
    Mat src_gray;
    Mat mDil;
    Mat mBlur;
    Mat mDiff;
    Mat merged;
    Mat mNorm;
    Mat mNormalized;
    Mat out;

    Mat channel[3];

    int max_binary_value = 255;

    try {
        src = imread(INPUT_STRING, IMREAD_COLOR);
        cout << "Dimension : "  << src.rows << "x" << src.cols << endl;
        cout << "Channels : "   << src.channels() << endl;
        merged.create(src.size(), src.type());

        split(src, channel);


        for (int i =0 ; i< 3; i++) {
            dilate(channel[i], mDil, Mat::ones(7,7, CV_8UC1), Point(-1, -1));
            medianBlur(mDil, mBlur, 21);
            absdiff(channel[i], mBlur, mDiff);
            mDiff = 255 - mDiff ;
            normalize(mDiff, mNorm, 0, 255, NORM_MINMAX, CV_8UC1);
            insertChannel(mNorm, merged, i);
        }

        cvtColor(merged, src_gray, COLOR_BGR2GRAY);

        imwrite(OUTPUT_STRING, merged);


        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        return -1;
    }
}

int grab_cut()
{
    Mat mat;
    Mat morph;
    Mat dst;
    try {
        mat = imread(INPUT_STRING, IMREAD_COLOR);
        Mat kernel (5,5, CV_8U, 1);

        morphologyEx(mat,morph, MORPH_CLOSE, kernel, Point(-1,-1), 3);


        Mat mask = Mat::zeros(mat.rows, mat.cols, CV_8UC1);
        Mat bgModel = Mat::zeros(1, 65, CV_64FC1);
        Mat fgModel = Mat::zeros(1, 65, CV_64FC1);
        Rect rect(20, 20, mat.rows - 10, mat.cols -10);

        grabCut( morph, mask, rect, bgModel, fgModel, 5, GC_INIT_WITH_RECT );

        Mat mask2 = (mask == 1) + (mask == 3);  // 0 = cv::GC_BGD, 1 = cv::GC_FGD, 2 = cv::PR_BGD, 3 = cv::GC_PR_FGD
        mat.copyTo(dst, mask2);

        imwrite(OUTPUT_STRING, dst);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}

int adaptive_threshold ()
{
    Mat src;
    Mat dst;
    Mat src_gray;
    Mat mDil;
    Mat mBlur;
    Mat mDiff;
    Mat merged;
    Mat mNorm;
    Mat mNormalized;
    Mat out;

    Mat channel[3];

    int max_binary_value = 255;

    try {
        src = imread(INPUT_STRING, IMREAD_COLOR);
        cout << "Dimension : "  << src.rows << "x" << src.cols << endl;
        cout << "Channels : "   << src.channels() << endl;
        merged.create(src.size(), src.type());

        split(src, channel);


        for (int i =0 ; i< 3; i++) {
            dilate(channel[i], mDil, Mat::ones(7,7, CV_8UC1), Point(-1, -1));
            medianBlur(mDil, mBlur, 21);
            absdiff(channel[i], mBlur, mDiff);
            absdiff(255, mDiff, mDiff);
            normalize(mDiff, mNorm, 0, 255, NORM_MINMAX, CV_8UC1);
            insertChannel(mNorm, merged, i);
        }

        cvtColor(merged, src_gray, COLOR_BGR2GRAY);

        dst.create(src_gray.size(), src_gray.type());

        fastNlMeansDenoising(src_gray, dst, 3, 7, 21);

        int blockSize = 501;
        int x1, y1 = 100;
        int x2 = x1 + blockSize;
        int y2 = y1 + blockSize;

        adaptiveThreshold(dst, dst, max_binary_value, ADAPTIVE_THRESH_GAUSSIAN_C, THRESH_BINARY, blockSize, 2);


        blur(dst, dst, Size(3,3));
        dilate(dst,dst, Mat() , Point(-1, -1), 2);
        erode(dst,dst, Mat() , Point(-1, -1), 2);

        imwrite(OUTPUT_STRING, dst);


        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        return -1;
    }
}


int canny_edge(int edgeThresh)
{
    Mat image;
    Mat dst;
    Mat cedge;
    Mat gray;
    Mat blurImage;
    Mat edge1;
    try {
        image = imread(INPUT_STRING, IMREAD_COLOR);
        cedge.create(image.size(), image.type());
        cvtColor(image, gray, COLOR_BGR2GRAY);
        blur (gray, blurImage, Size(3,3));
        Canny(blurImage, edge1, edgeThresh, edgeThresh*3, 3);

        image.copyTo(cedge,edge1);

        imwrite(OUTPUT_STRING, cedge);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}


int hough_lines(int edgeThresh, double minTheta, double maxTheta)
{
    Mat src;
    Mat dst;
    Mat cdst;
    ofstream myfile; 
    try {
        srand(time(NULL));
        src = imread(INPUT_STRING, IMREAD_GRAYSCALE);
        Canny(src, dst, edgeThresh, edgeThresh*3, 3);
        cvtColor(dst, cdst, COLOR_GRAY2BGR);

        vector<Vec2f> lines;
        HoughLines(dst, lines, 1, CV_PI/180, 150, 0, 0, minTheta, maxTheta);
        myfile.open(RHOS_FILENAME);

        for (size_t i= 0; i < lines.size(); i++){

            float rho = lines[i][0], theta = lines[i][1];
            myfile << rho << "," << theta << endl; 
            // cout << i << ": (" << rho << "," << theta << ")" << endl;
            Point pt1, pt2;
            double a = cos(theta), b = sin(theta);
            double x0 = a * rho, y0 = b * rho;
            pt1.x = cvRound(x0 + 1000 * (-b));
            pt1.y = cvRound(y0 + 1000 * (a));
            pt2.x = cvRound(x0 - 1000 * (-b));
            pt2.y = cvRound(y0 - 1000 * (a));
            line (cdst, pt1, pt2, Scalar(0,0,255), 3 , LINE_AA);
        }
        myfile.close();

        imwrite(OUTPUT_STRING, cdst);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}

int hough_lines_p(int edgeThresh, int voteThreshold, int minLineLength, int maxLineGap)
{
    Mat src;
    Mat dst;
    Mat cdstP;
    ofstream myfile; 
    try {
        srand(time(NULL));
        src = imread(INPUT_STRING, IMREAD_GRAYSCALE);
        Canny(src, dst, edgeThresh, edgeThresh*3, 3);
        cvtColor(dst, cdstP, COLOR_GRAY2BGR);

        vector<Vec4i> linesP;
        HoughLinesP(dst, linesP, 1, CV_PI/180, voteThreshold, minLineLength, maxLineGap);
        myfile.open(LINES_FILENAME);
        for (size_t i = 0; i < linesP.size(); i++) {
            Vec4i l = linesP[i];
            // cout << i << ": (" << l[0] << "," << l[1] << "), (" << l[2] << "," << l[3] << ")" << endl;
            myfile << l[0] << "," << l[1] << "," << l[2] << "," << l[3] << endl; 
            int rgb[] = {0,0,0};
            rgb[i%3] = 255;

            line(cdstP, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(rgb[0], rgb[1], rgb[2]), 3 , LINE_AA);
        }
        myfile.close();
        rectangle (cdstP, Point(0,0), Point (20,20), Scalar(0,0,255), 2);
        // for (int i= 1; i< 40; i++) {
        //     circle (cdstP, Point(0,0), i * 100, Scalar(0,0,255), 2);
        // }

        imwrite(OUTPUT_STRING, cdstP);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}


int hough_circles()
{
    Mat src;
    Mat gray; 
    Mat dst;
    try {
        src = imread(INPUT_STRING, IMREAD_COLOR);
        cvtColor(src, gray, COLOR_BGR2GRAY);
        medianBlur(gray, gray, 5);
        vector <Vec3f> circles;
        HoughCircles (gray, circles, HOUGH_GRADIENT, 1, gray.rows/16, 100, 30, 1, 30);
        for (size_t i = 0; i < circles.size() ; i++) {
            Vec3i c = circles[i];
            Point center = Point(c[0], c[1]);
            circle (src, center, 1, Scalar(0, 100, 100), 3, LINE_AA);
            int radius = c[2];
            circle (src, center, radius, Scalar(255, 0, 255), 3, LINE_AA);
        } 
        
        imwrite(OUTPUT_STRING, src);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}


int gaussian_blur(int mSize)
{
    Mat image;
    Mat fImage;
    Mat dst;
    Mat result;
    Mat output;
    try {
        image = imread(INPUT_STRING, IMREAD_GRAYSCALE);
        GaussianBlur(image, dst, Size(mSize, mSize) , 0, 0);
        divide(image, dst, result, 255);
        // result.convertTo(output, CV_8U, 255);
        imwrite(OUTPUT_STRING, result);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}

void* easy_init () {
    MyContainer* c = new MyContainer; 
    return c; 
}

int load_image(void* ptr, char* filename, int color_mode)
{
    try {
        MyContainer* c = static_cast<MyContainer*> (ptr);
        return c->load_image(filename, color_mode);
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}


int push_line(void* ptr, int x1, int x2, int y1, int y2)
{
    try {
        MyContainer* c = static_cast<MyContainer*> (ptr);
        c->push_line(x1, x2,  y1, y2);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}


int draw_lines(void* ptr)
{
    Mat src;
    Mat dst; 
    try {
        MyContainer* c = static_cast<MyContainer*> (ptr);
        vector<Vec4i> lines = c->get_lines();
        src = imread(INPUT_STRING, IMREAD_COLOR);
        for (int i=0; i < lines.size(); i++) {
            line(src, Point(lines[i][0], lines [i][1]), Point(lines[i][2], lines [i][3]),
                 Scalar(255,0,0), 3 , LINE_AA);
                 
        } 
        imwrite(OUTPUT_STRING, src);
        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}


int new_lines(void* ptr)
{
    try {
        MyContainer* c = static_cast<MyContainer*> (ptr);
        c->new_lines();
        return 0; 
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
    return 0;
}

int find_contours (int mSize, int edgeThresh)
{
    Mat src;
    Mat dst;
    Mat mBlur;
    Mat edge1;
    Mat cedge;
    Mat gray;

    vector <vector<Point> > contours; 
    vector< vector<Point> > contours0;

    vector <vector<Point> > final_contours; 
    vector<Vec4i> hierarchy;

    int max_idx = -1;
    float max_area = 0; 

    int max_binary_value = 255;
    double area; 

    try {
        src = imread(INPUT_STRING, IMREAD_COLOR);
        cedge.create(src.size(), src.type());
        cvtColor(src, gray, COLOR_BGR2GRAY);
        blur(gray, mBlur, Size(3, 3));
        Canny(mBlur, edge1, edgeThresh, edgeThresh*3, 3);

        src.copyTo(cedge, edge1);
        
        findContours(edge1, contours0, hierarchy, RETR_LIST, CHAIN_APPROX_SIMPLE);

        contours.resize(contours0.size());
        final_contours.resize(contours0.size());


        for( size_t k = 0; k < contours0.size(); k++ ) {
            approxPolyDP(Mat(contours0[k]), contours[k], 3, true);
            area = contourArea(contours[k]);

            if (area > 1000) {
                 // cout << k << " : "  << contours[k] << " ," <<  area << ", " << hierarchy[k] << endl;
                cout << area << "," << k << endl; 
            } 
        } 


        Mat cnt_img = Mat::zeros(src.rows, src.cols, CV_8UC3);

        int idx = 0;
        for( ; idx >= 0; idx = hierarchy[idx][0] ){
            Scalar color( rand()&255, rand()&255, rand()&255 );
            drawContours( cnt_img, contours, idx, color,3,  LINE_AA, hierarchy );
        }

        // drawContours(cnt_img, contours, -1, Scalar(128, 255,255),3, LINE_AA, hierarchy);
        
        imwrite(OUTPUT_STRING, cnt_img);


        return 0;
    } catch (cv::Exception e) {
        cout << "Caught Exception " << endl;
        cerr << e.what();
        return -1;
    }
}


// Driver code
int main()
{
    return 0;
}
