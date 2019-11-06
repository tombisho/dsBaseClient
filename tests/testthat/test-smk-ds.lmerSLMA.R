#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60", "noise.56", 
                                      "pm10.16", "bmi.26"))

#
# Tests
#

context("ds.lmerSLMA::smk::intercept")
test_that("simple lmerSLMA, intercept", {
    lmerSLMA.res <- ds.lmerSLMA('survtime ~female+(1|time.id', dataName = 'D')

    expect_length(lmerSLMA.res, 7)
    
})

#
# Done
#

disconnect.studies.dataset.cnsim()
