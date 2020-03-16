\name{datagvp4}
\alias{datagvp4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{raw data-set from Medtronic continuous glucose monitoring
%%  ~~function to do ... ~~
}
\description{
Raw data from type 1 diabetic patient acquired through Medtronic Paradigm Veo-754 continuous glucose monitoring.
}
%
\usage{
data("datagvp4")
}
%
% \begin{Arguments}
% \begin{ldescription}
%
% \end{ldescription}
% \end{Arguments}
%
\details{
Data-set with 12 days of data acquired with sampling time of 5 minutes. There are 4004 observations in total, containing two parts: from observation 1 to 737 data from the insulin pump, and from 738 to 4004 data from the sensor.
}
%
\format{
Data-set with data frame format containing forty seven columns:

  Index, Date (with format \emph{yyyy/mm/dd}), Time (with format \emph{hh:mm:ss}),
  New.Device.Time, BG.Reading..mg.dL., Linked.BG.Meter.ID,
  Basal.Rate..U.h., Temp.Basal.Amount, Temp.Basal.Type,
  Temp.Basal.Duration..h.mm.ss., Bolus.Type, Bolus.Volume.Selected..U.,
  Bolus.Volume.Delivered..U., Bolus.Duration..h.mm.ss., Prime.Type,
  Prime.Volume.Delivered..U., Alarm, Suspend, Rewind, BWZ.Estimate..U.,
  BWZ.Target.High.BG..mg.dL., BWZ.Target.Low.BG..mg.dL., BWZ.Carb.Ratio..U.Ex.,
  BWZ.Insulin.Sensitivity..mg.dL.U.,BWZ.Carb.Input..exchanges.,
  BWZ.BG.Input..mg.dL., BWZ.Correction.Estimate..U., BWZ.Food.Estimate..U.,
  BWZ.Active.Insulin..U., Sensor.Calibration.BG..mg.dL., Sensor.Glucose..mg.dL.,
  ISIG.Value, Event.Marker, Bolus.Number, Bolus.Cancellation.Reason,
  BWZ.Unabsorbed.Insulin.Total..U., Final.Bolus.Estimate, Scroll.Step.Size,
  Insulin.Action.Curve.Time, Sensor.Calibration.Rejected.Reason, Preset.Bolus,
  Bolus.Source, Network.Device.Associated.Reason,
  Network.Device.Disassociated.Reason, Network.Device.Disconnected.Reason,
  Sensor.Exception, Preset.Temp.Basal.Name.

}
%
% \begin{Author}\relax
%
% \end{Author}
%
%
\source{
Hospital Universitario Principe de Asturias de Alcala de Henares, Madrid, Spain.
}
%
% \begin{References}\relax
%
% \end{References}
%
\examples{
data("datagvp4")
datagvp4
}
