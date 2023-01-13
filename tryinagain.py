#%%
import numpy as np
import pandas as pd
from sklearn import tree
from sklearn.metrics import accuracy_score,confusion_matrix
from sklearn.model_selection import GridSearchCV
import matplotlib.pyplot as plt

results = []
alphas = []

params = {'max_depth': [2,4,6,8,10,12,14,16],
         'min_samples_split': [2,3,4,5,6,7,8],
         'min_samples_leaf': [1,2,3,4,5,6,7]}

#%%
data = pd.read_table("horse_data23.txt", header = 0)

#%%
lLeg = np.array(data["lameLeg"])
for i in range(1, len(lLeg)):
    if lLeg[i] == "right:fore" or lLeg[i] == "left:hind":
        lLeg[i] = "rflh"

    elif lLeg[i] == "left:fore" or lLeg[i] == "right:hind":
        lLeg[i] = "lfrh"

data["diag"] = lLeg

#%%

#X = pd.DataFrame((data["A"], data["W"]))
#X = pd.DataFrame((data["pc3"], data["pc4"]))
X = pd.DataFrame((data["A"], data["W"], data["pc3"], data["pc4"]))

horse = pd.factorize(data["horse"])
y = pd.factorize(data["lameLeg"])

X = np.array(X)
horse = np.array(horse[0])
y = np.array(y[0])

#%%
i = 1
indices = np.zeros(len(y))

for x in range(len(horse)):
    if horse[x] == i:
        indices[x] = 1

indices = np.transpose(indices)
X_train = np.transpose(X)[indices == 0]
X_test = np.transpose(X)[indices != 0]

y_train = y[indices == 0]
y_test = y[indices != 0]

print(np.shape(X_train))
print(np.shape(X_test))
print(np.shape(y_train))
print(np.shape(y_test))

#%%
clf = tree.DecisionTreeClassifier(random_state=0)
gcv = GridSearchCV(estimator=clf, param_grid=params)
fit = gcv.fit(X_train, y_train)
model = gcv.best_estimator_


fit = model.fit(X_train, y_train)
tree.plot_tree(fit)
print(gcv.best_estimator_)
#%%
print(gcv.best_params_)

#%%
bdepth = 6
bleaf = 3
bsplit = 2

#%%
model = tree.DecisionTreeClassifier(random_state=0, max_depth=bdepth, min_samples_leaf= bleaf, min_samples_split = bsplit)
path = model.cost_complexity_pruning_path(X_train, y_train)
ccp_alphas, impurities = path.ccp_alphas, path.impurities
clfs = []

print(ccp_alphas)

for ccp_alpha in ccp_alphas:
    clf = tree.DecisionTreeClassifier(random_state=0, ccp_alpha=ccp_alpha, max_depth=bdepth, min_samples_leaf= bleaf, min_samples_split = bsplit)
    clf.fit(X_train, y_train)
    clfs.append(clf)
clfs = clfs[:-1]
ccp_alphas = ccp_alphas[:-1]
#%%
train_acc = []
test_acc = []
for c in clfs:
    y_train_pred = c.predict(X_train)
    y_test_pred = c.predict(X_test)
    train_acc.append(accuracy_score(y_train_pred, y_train))
    test_acc.append(accuracy_score(y_test_pred, y_test))
print(train_acc)
plt.scatter(ccp_alphas,train_acc)
plt.scatter(ccp_alphas,test_acc)
plt.plot(ccp_alphas,train_acc,label='train_accuracy',drawstyle="steps-post")
plt.plot(ccp_alphas,test_acc,label='test_accuracy',drawstyle="steps-post")
plt.legend()
plt.title('Accuracy vs alpha')
plt.show()


#%%
chosen_alpha = 0.0325
b_model = tree.DecisionTreeClassifier(random_state=0, ccp_alpha=chosen_alpha, max_depth=bdepth, min_samples_leaf= bleaf, min_samples_split = bsplit)
b_fit = b_model.fit(X_train, y_train)
tree.plot_tree(b_fit)

y_pred = b_model.predict(X_test)

#%%
pred = fit.predict(X_test)
print(f'Test score {accuracy_score(y_pred,y_test)}')
cm = confusion_matrix(y_test, y_pred)
print(cm)

#%%
preds = []
for i in range(8):
    indices = np.zeros(len(y))
    for x in range(len(horse)):
        if horse[x] == i:
            indices[x] = 1
    X_train = np.transpose(X)[indices == 0]
    X_test = np.transpose(X)[indices != 0]

    y_train = y[indices == 0]
    y_test = y[indices != 0]

    clf =  tree.DecisionTreeClassifier(random_state=0, ccp_alpha=chosen_alpha)
    gcv = GridSearchCV(estimator=clf, param_grid=params)
    fit = gcv.fit(X_train, y_train)
    model = gcv.best_estimator_
    
    fit = model.fit(X_train, y_train)
    preds.append(fit.predict(X_test))
#%%
#print(preds)
alphas.append(chosen_alpha)
flatlist = []
for sublist in preds:
    for element in sublist:
        flatlist.append(element)

res = y == flatlist
res = res * 1
print(res)
print(np.mean(res) * 100)