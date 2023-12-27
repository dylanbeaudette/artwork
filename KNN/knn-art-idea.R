# https://github.com/davpinto/fastknn
# remotes::install_github("davpinto/fastknn")

library(fastknn)


## inspired by:
# https://twitter.com/adad8m/status/1737329823339737412
# https://goonlinetools.com/snapshot/code/#kh2ufemqqwbaw5eoq24mii

# number of particles
N <- 10

# dim of embedding
p <- 5      

# number of nearest neighbours to keep track of
NN_max <- N - 1

# param for mean field flow
beta <- 0.1 * sqrt(p)
dt <- 0.1
n_iter <- 100


# keep track of NN distance statistics
nearest_distance_stats <- list()

# initialize particles
X <- rnorm(n = N * p, mean = 0, sd = 1)
X <- matrix(X, ncol = p)

# X = X / np.linalg.norm(X, axis=1)[:,None]
# Frobenius norm
X.norm <- apply(X, 1, function(i) {
  norm(t(i), type = 'F')
})

X <- sweep(X, MARGIN = 1, STATS = X.norm, FUN = '/')

# for _ in range(n_iter):
for(i in 1:n_iter) {
  ## normalize
  X.norm <- apply(X, 1, function(i) {
    norm(t(i), type = 'F')
  })
  
  X <- sweep(X, MARGIN = 1, STATS = X.norm, FUN = '/')
  
  ## nearest distance stats with FAISS
  index = faiss.IndexFlatL2(p)
  index.add(X)
  D, _ = index.search(X, NN_max+1)
  
  ## compute mean distance to k-th NN
  # 2D array -> 1D array
  mean_distances = np.mean(D, axis=0)[1:]
  
  # nearest_distance_stats.append(mean_distances)
  nearest_distance_stats[[i]] <- mean_distances
  
  ## attention matrix
  # X_dots = X @ X.T
  X_dots <- X %*% t(X)
  
  # A = np.exp(beta*X_dots)
  A <- exp(beta * X_dots)
  
  # A = A / np.sum(A, axis=1)[:,None]
  A <- A / rowSums(A)
  
  
  ## vector field: Attention + tangent plane
  # Y = A @ X
  Y <- A %*% X
  
  # V = Y - np.sum(X*Y, axis=1)[:,None]*X
  V <- Y - rowSums(X * Y) * X
  
  ## particles update
  # X = X + dt*V
  X <- X + dt * V
}
  


## sweet colormap
# colors = [mcolors.to_rgba('yellow'), mcolors.to_rgba('red')]
# cmap = mcolors.LinearSegmentedColormap.from_list('yellow_to_red', colors, NN_max)

## plot!
# nearest_distance_stats = np.array(nearest_distance_stats)
# for k in range(NN_max):
#   plt.plot(nearest_distance_stats[:,k], color=cmap(k))
# plt.ylabel("Mean Distance to k-th NN", fontsize=15)







