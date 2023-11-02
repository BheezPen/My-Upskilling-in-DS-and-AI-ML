from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score

# Sample dataset (replace with your own)
X, y = ...

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Create a Decision Tree Classifier with a maximum depth (pruning)
max_depth = 5  # Set your desired maximum depth
clf = DecisionTreeClassifier(max_depth=max_depth)

# Fit (train) the model on the training data
clf.fit(X_train, y_train)

# Make predictions on the test data
y_pred = clf.predict(X_test)

# Calculate the accuracy of the pruned model
accuracy = accuracy_score(y_test, y_pred)
print(f"Accuracy (Pruned): {accuracy}")
