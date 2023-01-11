#%%
import numpy as np
import pandas as pd
from sklearn import tree
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score,confusion_matrix
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
import matplotlib.pyplot as plt
import csv

results = []



#%%
data = pd.read_table("horse_data23.txt", header = 0)

#X = pd.DataFrame((data["A"], data["W"]))
#X = pd.DataFrame((data["pc3"], data["pc4"]))
X = pd.DataFrame((data["A"], data["W"], data["pc3"], data["pc4"]))
y = pd.factorize(data["lameLeg"])

X = np.array(X)
y = np.array(y[0])

#%%
X_train, X_test, y_train, y_test = train_test_split(X.T, y, test_size = 0.1, random_state = 42)

params = {'max_depth': [2,4,6,8,10,12],
         'min_samples_split': [2,3,4,5,6,7,8],
         'min_samples_leaf': [1,2,3,4,5,6,7]}

#%%
# Pre-pruning 
clf = tree.DecisionTreeClassifier(random_state=0)
gcv = GridSearchCV(estimator=clf, param_grid=params)

fit = gcv.fit(X_train, y_train)
print(gcv.best_estimator_)
model = gcv.best_estimator_

#%%
fit = model.fit(X_train, y_train)

tree.plot_tree(fit)
pred = fit.predict(X_test)

#%%
print(f'Test score {accuracy_score(pred,y_test)}')
cm = confusion_matrix(y_test, pred)
print(cm)

#%%
# Post-pruning
path = clf.cost_complexity_pruning_path(X_train, y_train)
ccp_alphas, impurities = path.ccp_alphas, path.impurities
clfs = []

print(ccp_alphas)

for ccp_alpha in ccp_alphas:
    clf = tree.DecisionTreeClassifier(random_state=0, ccp_alpha=ccp_alpha)
    clf.fit(X_train, y_train)
    clfs.append(clf)

#%%
clfs = clfs[:-1]
ccp_alphas = ccp_alphas[:-1]
node_counts = [clf.tree_.node_count for clf in clfs]
depth = [clf.tree_.max_depth for clf in clfs]
plt.scatter(ccp_alphas,node_counts)
plt.scatter(ccp_alphas,depth)
plt.plot(ccp_alphas,node_counts,label='no of nodes',drawstyle="steps-post")
plt.plot(ccp_alphas,depth,label='depth',drawstyle="steps-post")
plt.legend()
plt.show()

#%%
train_acc = []
test_acc = []
for c in clfs:
    y_train_pred = c.predict(X_train)
    y_test_pred = c.predict(X_test)
    train_acc.append(accuracy_score(y_train_pred,y_train))
    test_acc.append(accuracy_score(y_test_pred,y_test))

plt.scatter(ccp_alphas,train_acc)
plt.scatter(ccp_alphas,test_acc)
plt.plot(ccp_alphas,train_acc,label='train_accuracy',drawstyle="steps-post")
plt.plot(ccp_alphas,test_acc,label='test_accuracy',drawstyle="steps-post")
plt.legend()
plt.title('Accuracy vs alpha')
plt.show()

#%%
b_model = tree.DecisionTreeClassifier(random_state=0, ccp_alpha=0.07)
b_fit = b_model.fit(X_train, y_train)
tree.plot_tree(b_fit)

y_pred = b_model.predict(X_test)

#%%
print(f'Test score {accuracy_score(y_test_pred,y_test)}')
cm = confusion_matrix(y_pred, y_test)
print(cm)

#%%
preds = np.zeros(len(data["A"]))

for i in range(len(data["A"])):
    indices = np.zeros(len(data["A"]))
    indices[i] = 1

    X_train = np.transpose(X)[indices == 0]
    X_test = np.transpose(X)[indices != 0]

    y_train = np.transpose(y)[indices == 0]
    y_test = np.transpose(y)[indices != 0]

    fit = b_model.fit(X_train, y_train)
    preds[i] = fit.predict(X_test.reshape(1,-1))

#%%
res = y == preds
res = res*1
print(res)
print(np.mean(res))

#%%
results.append(res)

#%%
np.savetxt("CT_res.csv", 
           results,
           delimiter =", ", 
           fmt ='% s')



# %%
