#!/usr/bin/env groovy

// =====================
// READ INPUT
// =====================

File inFile = new File("/uploads/input.txt")
if (!inFile.exists()) {
    println "ERROR: input.txt not found!"
    System.exit(1)
}

def lines = inFile.readLines()*.trim().findAll { it }
if (lines.size() < 3) {
    println "input.txt must contain at least 3 lines: width, height, maxIter"
    System.exit(1)
}

int width   = lines[0] as int
int height  = lines[1] as int
int maxIter = lines[2] as int


// =====================
// FRACTAL DISPLAY PARAMETERS
// =====================

// Center of Mandelbrot set (canonical)
double centerX = -0.5
double centerY =  0.0

double baseScale = 3.0      // height of view

double terminalAspect = width / (double) height

// Character aspect ratio (console characters are wider)
double charAR = 0.55        // tweak between 0.50–0.60 depending on font

// Real width must be adjusted by both terminal and character AR
double realWidth = baseScale * terminalAspect * charAR
double imagHeight = baseScale

// Compute view window
double minRe = centerX - realWidth / 2
double maxRe = centerX + realWidth / 2

double minIm = centerY - imagHeight / 2
double maxIm = centerY + imagHeight / 2


// =====================
// ASCII PALETTE
// =====================

String palette = " .,:;-+*xX#@"


// =====================
// MANDELBROT RECURSIVE FUNCTION
// =====================

int mandelRec(double zx, double zy, double cx, double cy, int iter, int maxIter) {
    if (zx*zx + zy*zy > 4.0 || iter >= maxIter) {
        return iter
    }
    double nx = zx*zx - zy*zy + cx
    double ny = 2*zx*zy + cy
    return mandelRec(nx, ny, cx, cy, iter + 1, maxIter)
}


// =====================
// RENDERING LOOP
// =====================

for (int j = 0; j < height; j++) {

    // Imaginary coordinate
    double cy = maxIm - (maxIm - minIm) * j / (height - 1)

    StringBuilder row = new StringBuilder()

    for (int i = 0; i < width; i++) {

        // Real coordinate (now perfectly centered!)
        double cx = minRe + (maxRe - minRe) * i / (width - 1)

        int iter = mandelRec(0.0d, 0.0d, cx, cy, 0, maxIter)

        if (iter >= maxIter) {
            row.append(palette[-1])  // inside set
        } else {
            int idx = (int)((iter / (double)maxIter) * (palette.length() - 1))
            row.append(palette[idx])
        }
    }

    println row.toString()
}
