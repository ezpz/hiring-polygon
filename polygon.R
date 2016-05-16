# Compute the area of a polygon
poly.area <- function(points) {
    xs <- points$X
    ys <- points$Y
    sum1 <- sum(xs[1:(length(xs) - 1)] * ys[2:length(ys)])
    sum2 <- sum(ys[1:(length(ys) - 1)] * xs[2:length(xs)])
    (sum1 - sum2) / 2
}

# Compute the length of a vertex of a polygon from the origin
poly.vertex.len <- function(poly, n) {
    x <- poly$X[n]
    y <- poly$Y[n]
    sqrt(x*x + y*y)
}

# Generate a data.frame that represents a polygon
# based on scores
poly.make <- function(scores) {
    n <- length(scores)
    scores <- scores/sum(scores)
    xs <- vector('numeric', n)
    ys <- vector('numeric', n)
    angles <- seq(0,2*pi,length.out=n+1)
    for (i in 1:n) {
        xs[i] <- scores[i] * cos(angles[i])
        ys[i] <- scores[i] * sin(angles[i])
    }
    df <- data.frame(matrix(c(xs, ys), ncol=2))
    names(df) <- c('X','Y')
    df
}

# Generate a random polygon based on N categories
poly.random <- function(ncats=8, points=100) {
    remain <- points
    ranks <- vector("numeric", ncats)
    ranks[] <- 0
    for (i in 1:(ncats - 1)) {
        x <- sample(remain * 0.6, 1) 
        ranks[i] <- x
        remain <- remain - x
    }
    ranks[ncats] <- remain
    poly.make (ranks)
}

poly.grid <- function(poly) {
    n <- length(poly$X)
    angles <- seq(0,2*pi, length.out=n+1)
    col <- "grey72"
    radius <- max(sqrt(poly$X*poly$X + poly$Y*poly$Y)) * 1.10
    segments(rep(0,n), rep(0,n), radius * cos(angles), radius * sin(angles), col=col)
}

poly.plot <- function(poly, col=rgb(0,0,0.5,alpha=0.5), add=FALSE, grid=FALSE) {
    if (! add) {
        bound <- max(sqrt(poly$X*poly$X + poly$Y*poly$Y)) * 1.10
        plot(poly$X, poly$Y, type='n', 
             xlim=c(-bound,bound), ylim=c(-bound,bound), 
             xlab="", ylab="", axes=FALSE)
        box();
        if(grid) { poly.grid(poly) }
    }
    polygon(poly$X, poly$Y, col=col)
}

poly.append <- function(poly, x, y) {
    rbind (poly, c(x,y))
}

# Find the intersection between (x3,y3)->(x4,y4) and (x1,y1)->(x2,y2)
line.intersect <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
    x <- ((x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*y4 - y3*x4)) / 
            ((x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4))
    y <- ((x1*y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3*y4 - y3*x4)) / 
            ((x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4))
    c(x,y)
}

# Given two polygons, compute the minimum distance vertex from 
# the origin for each vertex. Returns 1 at index i if p1 is 
# the minimum, 2 at index i otherwise
min.vertex <- function(p1, p2) {
    n <- length(p1$X)
    ps <- vector('numeric', n)
    for (i in 1:n) {
        l1 <- poly.vertex.len(p1, i)
        l2 <- poly.vertex.len(p2, i)
        ps[i] <- ifelse(l1 < l2, 1, 2)
    }
    ps
}

# Generate the intersection of the two polygons
# Much simpler than something like Weiler-Atherton because
# we have simplifying conditions of
#  equal number verticies in each polygon
#  both polygons centered on origin
poly.intersect <- function(p1, p2) {

    n <- length(p1$X)
    mins <- min.vertex(p1, p2)

    # Take the minimum length verticies at each index and build a 
    # new polygon from those. When there is a change in which polygon
    # has the minimum vertex, compute the intersection of the crossover
    # and add that as well.
    isect <- data.frame(matrix(ncol=2,nrow=0))
    
    last <- mins[1]
    for (i in 1:n) {
        if (last != mins[i]) {
            xy <- line.intersect(p1$X[i-1], p1$Y[i-1],
                                 p1$X[i], p1$Y[i],
                                 p2$X[i-1], p2$Y[i-1],
                                 p2$X[i], p2$Y[i])
            isect <- poly.append(isect, xy[1], xy[2])
        }

        if (mins[i] == 1) {
            isect <- poly.append(isect, p1$X[i], p1$Y[i])
        } else {
            isect <- poly.append(isect, p2$X[i], p2$Y[i])
        }
        last <- mins[i]
    }

    # 'connect' the last vertex to the first
    if (mins[n] != mins[1]) {
        xy <- line.intersect(p1$X[n], p1$Y[n],
                             p1$X[1], p1$Y[1],
                             p2$X[n], p2$Y[n],
                             p2$X[1], p2$Y[1])
        isect <- poly.append(isect, xy[1], xy[2])
    }
    names(isect) <- c('X','Y')
    isect
}

# [staff.current] is the CSV containing the mapping of our current staff to
# rankings in the expected categories of talent
# [staff.goal] is the CSV containing the mapping of what we'd like our staff
# to resemble in 5-10 years in the same categories
# [candidate] is the CSV containing her personal ranking in these dimensions 
compare.candidate <- function(staff.current, staff.goals, candidate) {
   # TODO: sample different areas
   # TODO: compare pairwise for largest and smalled overlap
   # TODO: median of overlap?
}
