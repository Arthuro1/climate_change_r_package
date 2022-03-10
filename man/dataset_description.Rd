\name{Climate US dataset}
\alias{Climate US dataset}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
  Climate USA Dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
This dataset contains values of temperature, heat waves index, drought severity index, co2 emissions and heavy precipitation area in the USA between the year 1901 and 2020.
}
\usage{data("climateset")}
\format{
  A data frame with the following 6 variables.
  \describe{
    \item{\code{Year}}{The different values are mesured from 1901 to 2020}
    \item{\code{drougth_severity_index_us}}{Drought Severity Index

The Palmer Drought Severity Index is the most widely used
index to measure drought severity over time. Positive values mean wetter than
average, negatives mean drier than average.}
    \item{\code{co2_emission_us}}{Annual Co2 Emissions

Based on ‘production’ or ‘territorial’ emissions (i.e. emissions from the burning of fossil fuels, or cement production within a country’s borders). It does not consider the emissions of traded goods (consumption-based emissions)}

    \item{\code{temperature_us}}{Contiguous U.S Average Temperature}

    \item{\code{heat_waves_index_us}}{Annual Heat Wave Index in the United States

    This index defines a heat wave as a period lasting at least four days with an average temperature that would only be expected to
occur once every 10 years, based on the historical record. The index value for a given year depends on how often heat waves occur
and how widespread they are.}

    \item{\code{heavy_precipitation_area_us}}{Heavy Precipitation Area

   This shows the percentage of the land area of the contiguous 48 states that experienced much greater than normal precipitation in any given year, which means it scored 2.0 or above on the annual Standardized Precipitation Index.

   }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\itemize{
\item{\href{https://www.epa.gov/climate-indicators/climate-change-indicators-us-and-global-temperature}{U.S. Temperature}}
\item{\href{https://ourworldindata.org/search?q=drought}{Drought Severity Index}}
\item{\href{https://ourworldindata.org/co2/country/united-states?country=~USA}{Annual Co2 Emissions}}
\item{\href{https://ourworldindata.org/search?q=heat+waves}{Annual Heat Wave Index}}
\item{\href{https://www.epa.gov/climate-indicators/climate-change-indicators-heavy-precipitation}{Heavy Precipitation Area}}
}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(climateset)
## maybe str(climateset) ; plot(climateset$temperature_us) ...
}
\keyword{datasets}
