# Installation des kernels pour jupyter notebook


## Authors

**Ményssa CHERIFA** [Linkedin](https://www.linkedin.com/in/menyssacherifa/) 

## Objectif

Utiliser  différents langages de programmation dans le même notebook ( script ).
Exemples disponibles : [Github](https://github.com/jupyter/jupyter/wiki/Jupyter-kernels)

## Kernel R

```
install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))

devtools::install_github('IRkernel/IRkernel')

IRkernel::installspec()

```

## Kernel Python 

```
sudo pip install -U jupyter
sudo apt install Python3
sudo pip3 install jupyter
sudo ipython3 kernelspec install-self
```




