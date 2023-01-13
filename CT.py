#%%
import numpy as np
import pandas as pd
from sklearn import tree
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score,confusion_matrix
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
import matplotlib.pyplot as plt

results = []
alphas = []
#%%
data = pd.read_table("horse_data23.txt", header = 0)

X = pd.DataFrame((data["A"], data["W"]))
#X = pd.DataFrame((data["pc3"], data["pc4"]))
#X = pd.DataFrame((data["A"], data["W"], data["pc3"], data["pc4"]))

y = pd.factorize(data["lameLeg"])
horse = pd.factorize(data["horse"])
horse = np.array(horse[0])
y = np.array(y[0])

print(y, horse)

X = np.array(X)
#%%

horses = ["B1", "B2", "B3", "B4", "B5", "B6", "B7", "B9"]

for x in range(len(horse)):
    if horse[x] == i:
        indices[x] = 1
print(indices)
X_train = np.transpose(X)[indices == 0]
print(X_train)
X_test = np.transpose(X)[indices != 0]

y_train = np.transpose(y)[indices == 0]
y_test = np.transpose(y)[indices != 0]

params = {'max_depth': [2,4,6,8,10,12,14,16],
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
clf = tree.DecisionTreeClassifier(random_state=0)
path = model.cost_complexity_pruning_path(X_train, y_train)
ccp_alphas, impurities = path.ccp_alphas, path.impurities
clfs = []

print(ccp_alphas)

for ccp_alpha in ccp_alphas:
    clf = tree.DecisionTreeClassifier(random_state=0, ccp_alpha=ccp_alpha)
    clf.fit(X_train, y_train)
    clfs.append(model)

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
chosen_alpha = 0.125
b_model = tree.DecisionTreeClassifier(random_state=0, ccp_alpha=chosen_alpha)
b_fit = b_model.fit(X_train, y_train)
tree.plot_tree(b_fit)

y_pred = b_model.predict(X_test)

#%%
print(f'Test score {accuracy_score(y_test_pred,y_test)}')
cm = confusion_matrix(y_pred, y_test)
print(cm)

#%%
#preds = np.zeros([len(data["A"]), 26])
preds = []

for i in range(5):
    indices = np.zeros(len(y))
    for x in range(len(y)):
        if i == y[x]:
            indices[x] = 1
    X_train = np.transpose(X)[indices == 0]
    X_test = np.transpose(X)[indices != 0]

    y_train = np.transpose(y)[indices == 0]
    y_test = np.transpose(y)[indices != 0]
    print(sum(indices))
    fit = b_model.fit(X_train, y_train)
    preds.append(fit.predict(X_test))

#%%
flat_list = [item for sublist in preds for item in sublist]
print(flat_list)
print(y)
#%%
res = y == flat_list
res = res*1
print(res)
print(np.mean(res))

#%%
results.append(res)
alphas.append(chosen_alpha)

#%%
np.savetxt("CT_res.csv", 
           results,
           delimiter =", ", 
           fmt ='% s')
# %%
