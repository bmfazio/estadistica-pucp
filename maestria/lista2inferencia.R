---
title: "Lista 2 - Inferencia"
author: "Boris Fazio"
date: "25 de noviembre de 2017"
output: html_document
---
  
2. La cantidad $X$ de soles que un comerciante agrega sobre el precio sugerido tiene la siguiente densidad:

$$\begin{align}
f_X(x)=\begin{cases}\theta x&,0< x \le 1\\
1-\frac{\theta}{2}&,1 < x \le 2\end{cases}
\end{align}$$

a) Halle el estimador de momentos de $\theta$

---

Calculamos el primer momento poblacional:

$$\begin{align}
\text{E}\left[X\right] &= \int_{-\infty}^{\infty}xf_X(x)dx\\
&= \int_{0}^{1}x\theta xdx + \int_{1}^{2}x\left(1-\frac{\theta}{2}\right)dx\\
&= \frac{\theta}{3} + \left(1-\frac{\theta}{2}\right)\frac{3}{2}\\
&= \frac{3}{2} - \frac{5}{12}\theta\\
\end{align}$$

Igualamos al primer momento muestral para obtener el estimador de momentos:

$$\begin{align}
\frac{3}{2} - \frac{5}{12}\hat\theta_{M} &= \bar X\\
\Rightarrow \hat\theta_{M} &= \frac{18-12\bar X}{5}
\end{align}$$

---

b) Determine si $\hat \theta_M$ es insesgado

---

$$\begin{align}
\text{E}\left[\hat\theta_M\right] &= \text{E}\left[\frac{18-12\bar X}{5}\right]\\
&= \frac{18-12\text{E}\left[\bar X\right]}{5}\\
&= \frac{18-12\left(\frac{3}{2} - \frac{5}{12}\theta\right)}{5}\\
&= \theta
\end{align}$$

Vemos que $\hat \theta_M$ es insesgado.

---

c) Halle el estimador de máxima verosimilitud de $\theta$

---



---
