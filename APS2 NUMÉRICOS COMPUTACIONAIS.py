# MÉTODOS NUMÉRICOS COMPUTACIONAIS
# 133R 
# Victor Hugo da Silva Vieira

import os
from PIL import Image 
import numpy as n

print("File location using os.getcwd():", os.getcwd())
os.chdir('D:\\Imagens\\Fotos')
print("File location using os.getcwd():", os.getcwd())

me1 = Image.open('1.jpg')
me2 = Image.open('2.jpeg')
 
img1 = n.asarray(me1)
img2 = n.asarray(me2) 

print(img1.shape)
print(img2.shape)

import numpy as n 
from numpy.linalg import svd

arr = n.array([[500, 500, 3], 
               [720, 586, 3]], 
              dtype = n.float32)
print("Array original:")
print(arr)

U, Singular, V = svd(arr)
print("-------------------------------")
print("Array: ",arr)
print("-------------------------------")
print("Valores Singulares:", Singular)
print("-------------------------------")
print("V^{T}",V.T)
