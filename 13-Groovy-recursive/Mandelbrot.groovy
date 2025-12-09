#!/usr/bin/env groovy

// Read width, height, maxIter from input.txt
File inFile = new File("input.txt")
if (!inFile.exists()) {
    println "ERROR: input.txt not found!"
    println "Please create a file with 3 lines: width, height, maxIter"
    System.exit(1)
}

def lines = inFile.readLines()*.trim().findAll { it }
if (lines.size() < 3) {
    println "ERROR: input.txt must contain >= 3 lines"
    println "Format:"
    println "width"
    println "height"
    println "maxIter"
    System.exit(1)
}

int width   = lines[0] as int
int height  = lines[1] as int
int maxIter = lines[2] as int

// Bounds of Mandelbrot complex plane
double minRe = -2.0
double maxRe =  1.0
double minIm = -1.0
double maxIm =  1.0

// ASCII palette: light → dark
String palette = " .,:;-+*xX#@"


// Recursive Mandelbrot escape function
int mandelRec(double zx, double zy, double cx, double cy, int iter, int maxIter) {
    if (zx*zx + zy*zy > 4.0 || iter >= maxIter) {
        return iter
    }
    double nx = zx*zx - zy*zy + cx
    double ny = 2*zx*zy + cy
    return mandelRec(nx, ny, cx, cy, iter + 1, maxIter)
}


// Print fractal to console
for (int j = 0; j < height; j++) {
    double cy = maxIm - (maxIm - minIm) * j / (height - 1)
    StringBuilder row = new StringBuilder()

    for (int i = 0; i < width; i++) {
        double cx = minRe + (maxRe - minRe) * i / (width - 1)
        int iter = mandelRec(0.0d, 0.0d, cx, cy, 0, maxIter)

        if (iter >= maxIter) {
            row.append(palette[-1])
        } else {
            int idx = (int)((iter / (double)maxIter) * (palette.length() - 1))
            row.append(palette[idx])
        }
    }

    println row.toString()
}
