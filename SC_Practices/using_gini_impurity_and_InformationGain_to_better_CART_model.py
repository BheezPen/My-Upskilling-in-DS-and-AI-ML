from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score

# Sample dataset (you should replace this with your own dataset)
data = [
    [25, 22000, 1],  # [Age, Income, Buy]
    [30, 25000, 0],
    [35, 35000, 1],
    [22, 18000, 0],
    [28, 24000, 1],
    [32, 38000, 0],
]

# Split the dataset into features (X) and target labels (y)
X = [row[:2] for row in data]  # Features: Age and Income
y = [row[2] for row in data]   # Target labels: Buy (1) or Not Buy (0)

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Create a Decision Tree Classifier with Gini impurity as the criterion
clf_gini = DecisionTreeClassifier(criterion='gini')

# Create a Decision Tree Classifier with Information Gain (entropy) as the criterion
clf_entropy = DecisionTreeClassifier(criterion='entropy')

# Fit (train) both models on the training data
clf_gini.fit(X_train, y_train)
clf_entropy.fit(X_train, y_train)

# Make predictions on the test data for both models
y_pred_gini = clf_gini.predict(X_test)
y_pred_entropy = clf_entropy.predict(X_test)

# Calculate the accuracy of both models
accuracy_gini = accuracy_score(y_test, y_pred_gini)
accuracy_entropy = accuracy_score(y_test, y_pred_entropy)

print(f"Accuracy (Gini): {accuracy_gini}")
print(f"Accuracy (Entropy): {accuracy_entropy}")
