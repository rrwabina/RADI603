# RADI603
Assignment

The emergence of infectious diseases with pandemic potential has served as a wake-up call to various governments across the globe. Througout human history, the biological threat of a pandemic disease has been devastating on an unprecedented scale, as evidenced by diseases such as COVID-19, influenza, and the bubonic plague. Because of these risks, the rapid spread of the Monkeypox disease this year has posed another global threat during COVID-19 pandemic. The Monkeypox, primarily caused by the monkeypox virus, is an infectious zoonotic disease which is closely related to smallpox but with milder infection. The World Health Organization (WHO) already declared the Monkeypox outbreak an international public health emergency, citing with a staggering increase of 30,000 cases worldwide. While the WHO has not declared the monkeypox oubreak a pandemic, several epidemiologists have highlighted the severity of monkeypox disease infection among the human population.

The transmission of monkeypox virus occurs when a person comes into contact with an infected animal or human. The virus is highly susceptible (i.e., able to contract the disease) to various animal species, particularly on non-human primates (e.g. monkeys). Animal-to-human transmission occurs primarily when infected monkeys transmit the monkeypox virus via bite, scratch, or even contaminated monkey meat preparation. Human-to-human transmission also occurs when infected humans pass the virus on to other people through direct contact with bodily fluids (i.e., sweat, semen, blood). Those who are susceptible can be exposed to the virus (i.e., have been infected but are not yet infectious) and will become infected (i.e., capable of transmitting the disease to other humans) at any time. Unfortunately, there is no known monkeypox vaccine in circulation, but smallpox vaccine may be used as a protection against Monkeypox with 85% effectivity. Nonetheless, while most infected people may recover from Monkeypox, the WHO highly recommends infected patients to take vaccine, especially if symptoms are severe.


#### 1.1 Derivatives in Disease Modeling

Controlling infectious diseases is a difficult task because the behavior of virus transmission among animal and human populations changes over time. Consider the following example to better understand the dynamics of virus transmission. Assume that the monkeypox virus has infected only one animal. This infected animal may scratch two people, exposing them to the virus, which may infect other people as well. Through direct contact, the infected humans may spread the Monkeypox virus to the other two people. Each person can then infect two more people, and so on. Because only one monkey is infected and humans can only infect two people, epidemiologists will be able to easily track virus transmission. However, how can epidemiologists track the spread of the Monkeypox virus if the source of infection is four infected monkeys? What if infected people spread the virus to four or six people? What if the virus evolves into a more potent mutation? In practice, these changes will be difficult to monitor because epidemiologists must account for them in order to explain the dynamics of virus transmission.  

Ordinary differential equations (ODEs) are the primary tools used to investigate changes in virus transmission. In disease modeling, these ODEs are known as epidemiological models that use compartments, such as susceptible $S$, infected $I$, recovered $R$, and etc., to explain the instantaneous change in the number of infected people over a fixed time $t$ - that is, a change in functions $S(t)$, $I(t)$, and $R(t)$. Because ODEs can examine a changing system (i.e., virus transmission) that includes the derivative of a function (e.g., change in number of infected humans), as well as the function (e.g., number of infected humans), these epidemiological models can be used to evaluate the long-term incidence of disease transmission among specific populations (e.g., number of infected humans). 

The key goal of epidemiological models is to calculate the long-term incidence of a disease. It refers to the number of people infected with a disease during a specific time period. It also looks into population behavior in terms of key epidemiological parameters (e.g., susceptible, infected, recovered) by examining when and how the epidemic will begin and end. Epidemiologists employ ODEs to track the number of infected people based on changes in other populations to solve long-term incidences. In the event of an epidemic or pandemic, this strategy will also assist local government units in implementing safety protocols and public health interventions. 

#### 1.2 Example Problem and Solution
In this example, we use the classical SIR model to analyze the spread of a disease. Suppose $S(t)$, $I(t)$, and $R(t)$ be the number of susceptible, infected, and recovered humans, respectively. The time $t$ is the number of days since the disease was discovered in an isolated town with a population of $N = 10,000$ people. 

Every SIR model requires parameters that characterize the change in the population of the compartments. In this example, we used three primary parameters, i.e., $\beta$, $\nu$, and $\gamma$. Suppose $\beta$ describes the transmission rate of the disease - that is, susceptible individual is in direct contact with an infected human. We also presume that $\gamma$ is the mean recovery rate and $\nu$ is the death rate of individuals caused by the disease. All parameters are assumed to be greater than zero. In addition, an individual is immuned and can no longer be susceptible when a person is recovered from the disease. For the sake of simplicity, this example ignores other parameters like the birth parameter $\Lambda$ and non-disease deaths $\nu$. 

Our goal in this problem is to develop the differential equations based from the given assumptions to make predictions about the effects of the disease on the population. We apply the concepts of derivatives (rate of change) to formulate the differential equation to every compartment. The number of susceptible individuals $S_h(t)$ will decrease over time at a transmission rate $\beta$ proportional to the product of the number of susceptibles $S$ and number of infected $I$. Its differential equation is given as:
\begin{equation*}
    \frac{dS}{dt} = - \beta S(t)I(t) \\
    \tag{1}
\end{equation*}

The number of the infected people changes (i.e., $\frac{dI}{dt}$) increases with the transmission rate $\beta$ at which susceptible individuals become infectious. It is decreased by the death rate caused by the virus $\nu$, and the recovered individuals after infection due to recovery rate $\gamma$. Hence, the rate of change in the population of infected humans is:
\begin{equation*}
    \frac{dI}{dt} = \beta S(t)I(t) - (\nu + \gamma) I(t)
    \tag{2}
\end{equation*}

Finally, the number of recovered human population increases as a result of the transition from being clinically ill to the recovered compartments at the rate $\gamma$. Therefore, the rate of change in the population of recovered humans is:
\begin{equation*}
    \frac{dR}{dt} =  \gamma I(t)
    \tag{3} 
\end{equation*}


We can adopt the Euler's method to approximate the systems of equations above. Let $S(t)$, $I(t)$, and $R(t)$ be $S_t$, $I_t$, and $R_t$, respectively, to denote the number of days that have passed. We approximate $\frac{dS}{dt}$ by realizing that $\Delta S = S_{t+1} - S_t$. We also utilize the similar process for other compartments. This will result into the following:

\begin{equation*}
\begin{aligned}
    \Delta S        &= S_{t+1} - S_t \\
    - \beta S_tI_t    &= S_{t+1} - S_t \\
    S_{t+1}         &= S_t - \beta S_tI_t
    \tag{4}
\end{aligned}
\end{equation*}

\begin{equation*}
\begin{aligned}
    \Delta I        &= I_{t+1} - I_t \\
    \beta S_tI_t - (\nu + \gamma) I_t  &= I_{t+1} - I_t\\
    I_{t+1} &= I_t + \beta S_tI_t - (\nu + \gamma) I_t \\
    I_{t+1} &= I_t(1 + \beta S_t - (\nu + \gamma)) \\
    \tag{5}
\end{aligned}
\end{equation*}


\begin{equation*}
\begin{aligned}
    \Delta R        &= R_{t+1} - R_t \\
    \gamma I_t  &= R_{t+1} - R_t\\
    R_{t+1} &= R_t + \gamma I_t
    \tag{6}
\end{aligned}
\end{equation*}

Therefore, equations derived using Euler's method are expressed in Equation 7:

\begin{align*}
    S_{t+1} & = S_t - \beta S_tI_t \\
    I_{t+1} & = I_t(1 + \beta S_t - \gamma -\nu)\\
    R_{t+1} & = R_t + \gamma I_t
    \tag{7}
\end{align*}
