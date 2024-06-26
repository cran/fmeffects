% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictorParsnip.R
\name{PredictorParsnip}
\alias{PredictorParsnip}
\title{PredictorParsnip}
\description{
This task specializes \link{Predictor} for \code{parsnip} models.
The \code{model} is assumed to be a \code{model_fit} object.

It is recommended to use \code{\link[=makePredictor]{makePredictor()}} for construction of Predictor objects.
}
\section{Super class}{
\code{\link[fmeffects:Predictor]{fmeffects::Predictor}} -> \code{PredictorParsnip}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictorParsnip-new}{\code{PredictorParsnip$new()}}
\item \href{#method-PredictorParsnip-predict}{\code{PredictorParsnip$predict()}}
\item \href{#method-PredictorParsnip-clone}{\code{PredictorParsnip$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictorParsnip-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictorParsnip-new}{}}}
\subsection{Method \code{new()}}{
Create a new PredictorParsnip object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictorParsnip$new(model, data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model}}{\code{model_fit} object.}

\item{\code{data}}{The data used for computing FMEs, must be data.frame or data.table.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictorParsnip-predict"></a>}}
\if{latex}{\out{\hypertarget{method-PredictorParsnip-predict}{}}}
\subsection{Method \code{predict()}}{
Predicts on an observation \code{"newdata"}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictorParsnip$predict(newdata)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{newdata}}{The feature vector for which the target should be predicted.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictorParsnip-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictorParsnip-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictorParsnip$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
