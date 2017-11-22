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
\frac{3}{2} - \frac{5}{12}\hat\theta_{\text{MM}} &= \bar X\\
\Rightarrow \hat\theta_{\text{MM}} &= \frac{18-12\bar X}{5}
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

Hallamos la log-verosimilitud y sus derivadas para una muestra aleatoria:

$$\begin{align}
\ell(x_1,...,x_n\mid\theta)&= \sum_{i=1}^n \log f(x_i\mid\theta)\\
&= \sum_{x_i \in (0,1])} \log \theta x_i + \sum_{x_i \in (1,2])} \log (1-\frac{\theta}{2})\\
\Rightarrow \ell' &= \sum_{x_i \in (0,1])} \frac{1}{\theta} - \sum_{x_i \in (1,2])} \frac{1}{2-\theta}\\
&= \frac{n_{(0,1]}}{\theta} - \frac{n_{(1,2]}}{2-\theta}\\
\Rightarrow \ell'' &= -\frac{n_{(0,1]}}{\theta^2} - \frac{n_{(1,2]}}{(2-\theta)^2}
\end{align}$$

Buscamos puntos críticos de $\ell$ para obtener el estimador de máxima verosimilitud:

$$\begin{align}
\frac{n_{(0,1]}}{\theta_{\text{MV}}} - \frac{n_{(1,2]}}{2-\theta_{\text{MV}}} &= 0\\
\Rightarrow \hat\theta_{\text{MV}} &= \frac{2}{1+\frac{n_{(1,2]}}{n_{(0,1]}}}
\end{align}$$

---
