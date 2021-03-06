% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch.spikes.R
\name{batch.spikes}
\alias{batch.spikes}
\title{batch_spikes}
\usage{
batch.spikes(rawdata, std.factor = 1, sigma = NA)
}
\arguments{
\item{rawdata}{ephys data to work on}

\item{std.factor}{depending on the quality of the data. A value of 1 is a good value for data with a really good signal to noise ratio.}

\item{sigma}{not yet implemented}
}
\value{
processed spike data
}
\description{
This file is part of the rephys package.
}
\details{
Copyright(c) Martin Rüßler

This file may be licensed under the terms of of the
GNU General Public License Version 3 (the ``GPL''),
or (at your option) any later version.

Software distributed under the License is distributed
on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
express or implied. See the GPL for the specific language
governing rights and limitations.

You should have received a copy of the GPL along with this
program. If not, go to http://www.gnu.org/licenses/gpl.html
or write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

The development of this software was supported by the
Excellence Cluster EXC 277 Cognitive Interaction Technology.
The Excellence Cluster EXC 277 is a grant of the Deutsche
Forschungsgemeinschaft (DFG) in the context of the German
Excellence Initiative.

\code{batch_spikes} more documentation might come in the future
steps: »unpack« the rawdata into filenames and data. find spikes in the data streams pack the data into one array
}

