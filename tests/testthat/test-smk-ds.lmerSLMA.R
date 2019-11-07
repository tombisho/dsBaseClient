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

connect.studies.dataset.cluster(list("BMI", "trtGrp", "Male", "idSurgery", "idDoctor", 
                                      "idPatient", "private"))

#
# Tests
#

context("ds.lmerSLMA::smk::intercept")
test_that("simple lmerSLMA, intercept", {
    lmerSLMA.res <- ds.lmerSLMA('BMI ~ trtGrp + Male + (1|idSurgery)', dataName = 'D')

    expect_length(lmerSLMA.res, 7)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_equal(class(glmSLMA.res$betamatrix.all), "matrix")
    expect_equal(class(glmSLMA.res$sematrix.all), "matrix")
    expect_equal(class(glmSLMA.res$betamatrix.valid), "matrix")
    expect_equal(class(glmSLMA.res$sematrix.valid), "matrix")
    expect_equal(class(glmSLMA.res$SLMA.pooled.ests.matrix), "matrix")
    expect_length(glmSLMA.res$output.summary, 5)
    expect_equal(class(glmSLMA.res$output.summary$input.beta.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$output.summary$input.se.matrix.for.SLMA), "matrix")
    expect_length(glmSLMA.res$output.summary$study1, 17)
    
})

#
# Done
#

disconnect.studies.dataset.cluster()
