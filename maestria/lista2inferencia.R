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

b) Determine si $\hat \theta_{MM}$ es insesgado

---

$$\begin{align}
\text{E}\left[\hat\theta_{MM}\right] &= \text{E}\left[\frac{18-12\bar X}{5}\right]\\
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
Dado que $\ell''$ es negativa para todo valor de $\theta$, los puntos críticos de $\ell$ corresponden a un máximo. Buscamos estos puntos para obtener el estimador de máxima verosimilitud:
$$\begin{align}
&\frac{n_{(0,1]}}{\hat\theta_{\text{MV}}} - \frac{n_{(1,2]}}{2-\hat\theta_{\text{MV}}} = 0\\
\Rightarrow &\hat\theta_{\text{MV}} = \frac{2}{1+\frac{n_{(1,2]}}{n_{(0,1]}}}\\
&\hat\theta_{\text{MV}} = \frac{2}{1+\frac{n_{(1,2]}}{n-n_{(1,2]}}}\\
&\hat\theta_{\text{MV}} = 2\left(1 - \frac{n_{(1,2]}}{n}\right)\\ 
\end{align}$$

---

d) Determine si $\hat\theta_{\text{MV}}$ es un estimador insesgado y consistente de $\theta$

---

$$\begin{align}
\text{E}\left[\hat\theta_{MV}\right] &= \sum_{i=0}^{n}\hat\theta_{MV}\text{P}(n_{(1,2]}=i\mid\theta)\\
&=\sum_{i=0}^{n}2\left(1 - \frac{i}{n}\right)\text{P}(n_{(1,2]}=i\mid\theta)\\
&=2\left(\sum_{i=0}^{n}\text{P}(n_{(1,2]}=i\mid\theta) - \frac{1}{n}\sum_{i=0}^{n}i{n \choose i}\left(1-\frac{\theta}{2}\right)^i\left(\frac{\theta}{2}\right)^{n-i}\right)\\
&=2\left(1 - \frac{1}{n}n(1 - \frac{\theta}{2})\right)\\
&=\theta
\end{align}$$

---

e) Bajo el modelo considerado, estime $\theta$ para una marca de shampoo con precio sugerido de 14 soles a partir de los siguientes valores observados:

```{r}
datos <- c(13.8,14.5,14,12.99,15,16)
```

Determine la mejor estimación bajo los métodos considerados.

---

Calculamos los $X$ observados:

```{r}
x <- datos-14
```
```{r,echo=FALSE}
x
```

Bajo el modelo considerado, $\text{P}(X \le 0) = 0$, por lo que descartamos las observaciones que no se ajustan durante la estimación.

```{r}
x <- x[x>0]
# Estimacion MM
(18 - 12*mean(x))/5
# Estimacion MV
2*(1-length(x[x>1])/length(x))
```

Las estimaciones no coinciden. Ya que ambos estimadores son insesgados, comparamos sus varianzas:

$$\begin{align}
\text{V}\left[\hat\theta_{MM}\right] &= \text{V}\left[\frac{18-12\bar X}{5}\right]&\text{V}\left[\hat\theta_{MV}\right] &= \text{V}\left[2\left(1 - \frac{n_{(1,2]}}{n}\right)\right]\\
&= \frac{144}{25}\text{V}\left[\bar X\right]&&= \frac{4}{n^2}\text{V}\left[n_{(1,2]}\right]\\
&= \frac{144}{25}\frac{\text{V}\left[X\right]}{n}&&\text{Var de v.a. binomial: } np(1-p)\\
&= \frac{144}{25}\frac{1}{n}\left(\text{E}\left[X^2\right]-\text{E}\left[X\right]^2\right)&&= \frac{4}{n^2}n\left(1-\frac{\theta}{2}\right)\frac{\theta}{2}\\
&= \frac{144}{25}\frac{1}{n}\left(\int_0^1x^2\theta xdx + \int_1^2x^2\left(1-\frac{\theta}{2}\right)dx - \left(\frac{3}{2} - \frac{5}{12}\theta\right)^2\right)&&= \frac{1}{n}\left(2\theta - \theta^2\right)\\
&= \frac{1}{n}\left(\frac{48}{25}\theta + \frac{12}{25} - \theta^2 \right)&\\
\end{align}$$

$$\begin{align}
\text{V}\left[\hat\theta_{MM}\right] - \text{V}\left[\hat\theta_{MV}\right] &= \frac{1}{n}\left(\frac{48}{25}\theta + \frac{12}{25} - \theta^2 \right) -  \frac{1}{n}\left(2\theta - \theta^2\right)\\
&= \frac{1}{n}\left(\frac{12-2\theta}{25}\right)\\
\Rightarrow \text{V}\left[\hat\theta_{MM}\right] > \text{V}\left[\hat\theta_{MV}\right] &\text{ para } \theta \in [0,2]
\end{align}$$

Por lo cual consideramos que $\hat\theta_{MV} = 4/3$ es el mejor estimado.

---

f) Asumiendo correcto el modelo, estime la probabilidad de que en un mercado el frasco se venda a menos de 15 soles

---

Calculamos la probabilidad deseada con la estimación MV del problema anterior:

$$\begin{align}
\text{P}\left(X<1\mid \theta=\frac{4}{3}\right) &= \int_{-\infty}^1 f_X\left(x\mid\theta=\frac{4}{3}\right)dx\\
&= \int_{0}^1 x\frac{4}{3}dx\\
&= \frac{2}{3}
\end{align}$$

---

3. Halle los estimadores de máxima verosimilitud para los $p_h, h = 1,...,k$ asociados a un vector aleatorio $(X_1,..,X_k) \sim \text{Multinomial}(n,p_1,...,p_k)$

---

Asumimos que $n$ es conocido. Procedemos planteando la log-verosimilitud:

$$\begin{align}
\ell(x_1,...,x_k\mid n,p_1,...,p_k) &= \log\left(\frac{n!}{x_1!...x_k!}p_1^{x_1}...p_k^{x_k}\right)\\
&=\log n! - \sum_{i=1}^k\log x_i! + \sum_{j=1}^k x_j\log p_j\\
\end{align}$$

Empleamos el Lagrangiano para obtener el EMV sujeto a la restricción $\sum_{i=1}^k p_i = 1$:

$$\begin{align}
\mathcal{L}(p_1,...p_k) &= \log n! - \sum_{i=1}^k\log x_i! + \sum_{i=1}^k x_i\log p_i - \lambda(\sum_{i=1}^kp_i - 1)\\
\Rightarrow \frac{\partial\mathcal{L}}{\partial p_h} &= \frac{x_h}{p_h} - \lambda\quad,\quad\frac{\partial\mathcal{L}}{\partial \lambda} = \sum_{i=1}^kp_i - 1\\
\Rightarrow \hat p_{h\text{MV}} &= \frac{x_h}{\sum_{i=1}^kx_i}
\end{align}$$

---

4. Considere un modelo de la forma $Y_j = \beta x_j + \epsilon_j$ con $\text{E}(\epsilon_j) = 0$, $\text{V}(\epsilon_j) = \sigma^2 x_j^p$ y $p$ conocido
a) Halle el MELI de $\beta$

---

$$\begin{align}
\text{E}\left[\hat\beta\right] &= \text{E}\left[\sum_{i=1}^na_iY_i\right]&\text{V}\left[\hat\beta\right] &= \text{V}\left[\sum_{i=1}^na_iY_i\right]\\
&= \sum_{i=1}^na_i\text{E}\left[\beta x_i + \epsilon_i\right]&&= \sum_{i=1}^na_i^2\text{V}\left[\beta x_i + \epsilon_i\right]\\
&= \beta\sum_{i=1}^na_ix_i&&= \sigma^2\sum_{i=1}^na_i^2x_i^p\\
\end{align}$$

Para obtener el MELI, requerimos que $\hat\beta$ sea insesgado lo que implica $\sum_{i=1}^na_ix_i = 1$. Además, buscamos los $a_i$ que minimicen la varianza:

$$\begin{align}
\mathcal{L}\left(a_i\right) &= \text{V}\left[\hat\beta\right] - \lambda\left(\sum_{i=1}^na_ix_i - 1\right)\\
\Rightarrow \nabla\mathcal{L} &= \left[
\begin{array}{cccc|c}
  \frac{\sigma^2}{2}x_1^p&0&\cdots&-x_1&0\\
  \vdots&\ddots&&\vdots&\vdots\\
  0&&\frac{\sigma^2}{2}x_n^p&-x_n&0\\
  x_1&\cdots&x_n&0&1\\
\end{array}
\right]\\
&= \left[
\begin{array}{cccc|c}
  \frac{\sigma^2}{2}x_1^p&0&\cdots&-x_1&0\\
  \vdots&\ddots&&\vdots&\vdots\\
  0&&\frac{\sigma^2}{2}x_n^p&-x_n&0\\
  0&\cdots&0&\frac{2}{\sigma^2}\sum_{j=1}^nx_j^{2-p}&1\\
\end{array}
\right]\\
&= \left[
\begin{array}{cccc|c}
  \frac{\sigma^2}{2}x_1^p&0&\cdots&0&\frac{\sigma^2}{2}\frac{x_1}{\sum x_j^{2-p}}\\
  \vdots&\ddots&&\vdots&\vdots\\
  0&&\frac{\sigma^2}{2}x_n^p&0&\frac{\sigma^2}{2}\frac{x_n}{\sum x_j^{2-p}}\\
  0&\cdots&0&\frac{2}{\sigma^2}\sum x_j^{2-p}&1\\
\end{array}
\right]\\
\end{align}$$
$$\begin{align}
\Rightarrow a_i &= \frac{x_i^{1-p}}{\sum x_j^{2-p}}\\
\Rightarrow \hat\beta_{\text{MELI}} &= \left(\sum_{j=1}^n x_j^{2-p}\right)^{-1}\sum_{i=1}^n y_i x_i^{1-p}\\
\end{align}$$

---

b) Halle el EMC para $\beta$ y su sesgo

---



---

c) Si se asumen errores normales, halle los EMV de $\beta$ y $\sigma^2$

---

---

5. Considere el siguiente modelo de regresión múltiple no lineal:
$$Y_j = g(x_j,\beta)+\epsilon_j,\quad j=1,...,n$$
donde $g : \mathbb{R}^d \rightarrow \mathbb{R}$ es una función con segundas derivadas parciales continuas, $x_j$ un vector $p$-dimensional de variables no aleatorias independientes y $\epsilon_j \sim \text{N}(0,\sigma^2)$. Se asume que los errores son independientes.
a) Muestre que el EMV de $\sigma^2$ viene dado por:
$$\hat\sigma^2 = \frac{1}{n}\sum_{j=1}^n \left(Y_j - g(x_j,\hat\beta)\right)^2,$$
donde $\hat\beta$ es el EMV de $\beta$

---

---
