% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bankr.R
\name{account}
\alias{account}
\title{R6 Class for a bank account}
\description{
A bank account with basic functionality for adding or removing money from its
balance.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{balance}}{the account balance}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-deposit}{\code{account$deposit()}}
\item \href{#method-withdraw}{\code{account$withdraw()}}
\item \href{#method-clone}{\code{account$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-deposit"></a>}}
\if{latex}{\out{\hypertarget{method-deposit}{}}}
\subsection{Method \code{deposit()}}{
Method to deposit money.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{account$deposit(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{The amount of money to add  to the account or to withdraw from
the account.
@export}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The current balance after performing the transaction.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-withdraw"></a>}}
\if{latex}{\out{\hypertarget{method-withdraw}{}}}
\subsection{Method \code{withdraw()}}{
Method to withdraw money.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{account$withdraw(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{The amount of money to add  to the account or to withdraw from
the account.
@export}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The current balance after performing the transaction.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{account$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
