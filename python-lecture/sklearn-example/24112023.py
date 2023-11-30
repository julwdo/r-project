from sklearn.datasets import make_blobs, load_iris, load_digits
import matplotlib.pyplot as plt

X, y = make_blobs(n_samples = 1000, n_features = 2, centers = 4, cluster_std = 2)

# X contains points
# y contains cluster assignment

print(X)
print(y)

fig, ax = plt.subplots()
ax.scatter(X[:, 0], X[:, 1], c = y, alpha = .6)
plt.tight_layout()
plt.show()

data = load_iris()
print(data.data)
print(data.target)
print(data.target_names)

digits = load_digits()
print(digits)

plt.gray()
plt.matshow(digits.images[2])
plt.show()

