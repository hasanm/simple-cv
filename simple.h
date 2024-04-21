#ifndef SYSTEM_INFO_H
#define SYSTEM_INFO_H

#ifdef __cplusplus
extern "C" {
#endif
    char* get_machine();
    int cut_image(int x, int y, int p, int q);
    int draw_rectangle(int x, int y, int p, int q);
    int copy_image(char *filename);
    int paste_image(char *filename);
    int adaptive_threshold();
    int my_merge();
    int make_gray();
    int grab_cut();
    int canny_edge(int edgeThresh);
    int hough_lines(int edgeThresh, double minTheta, double maxTheta);
    int hough_lines_p(int edgeThresh, int minLineLength, int maxLineGap);

#ifdef __cplusplus
}
#endif

#endif
