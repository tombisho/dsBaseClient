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
                                      "idPatient", "private", "diabetes", "incid_rate"))

#
# Tests
#

context("ds.glmerSLMA::smk::poisson")
test_that("simple glmerSLMA, intercept, poisson", {
    glmerSLMA.res <- ds.glmerSLMA('incid_rate ~ trtGrp + Male + (1|idSurgery) + (1|idDoctor)', 
                                family = 'poisson', dataName = 'D')

    expect_length(glmerSLMA.res, 7)
    expect_equal(glmerSLMA.res$num.valid.studies, 3)
    expect_equal(class(glmerSLMA.res$betamatrix.all), "matrix")
    expect_equal(class(glmerSLMA.res$sematrix.all), "matrix")
    expect_equal(class(glmerSLMA.res$betamatrix.valid), "matrix")
    expect_equal(class(glmerSLMA.res$sematrix.valid), "matrix")
    expect_equal(class(glmerSLMA.res$SLMA.pooled.ests.matrix), "matrix")
    expect_length(glmerSLMA.res$output.summary, 5)
    expect_equal(class(glmerSLMA.res$output.summary$input.beta.matrix.for.SLMA), "matrix")
    expect_equal(class(glmerSLMA.res$output.summary$input.se.matrix.for.SLMA), "matrix")
    expect_length(glmerSLMA.res$output.summary$study1, 20)
    expect_equal(class(glmerSLMA.res$output.summary$study1$coefficients), "matrix")
    expect_equal(as.numeric(glmerSLMA.res$output.summary$study1$AICtab[1]), 5926.437, tolerance = 0.001)
    expect_equal(as.numeric(glmerSLMA.res$output.summary$study1$devcomp$dims[1]), 1050)
    expect_equal(as.numeric(glmerSLMA.res$output.summary$study1$ngrps[2]), 8)
})

context("ds.glmerSLMA::smk::binomial")
test_that("simple glmerSLMA, intercept, binomial", {
    glmerSLMA.res <- ds.glmerSLMA('diabetes ~ trtGrp + Male + (1|idSurgery) + (1|idDoctor)', 
                                 family = 'binomial', dataName = 'D')
    
    expect_length(glmerSLMA.res, 7)
    expect_equal(glmerSLMA.res$num.valid.studies, 3)
    expect_equal(class(glmerSLMA.res$betamatrix.all), "matrix")
    expect_equal(class(glmerSLMA.res$sematrix.all), "matrix")
    expect_equal(class(glmerSLMA.res$betamatrix.valid), "matrix")
    expect_equal(class(glmerSLMA.res$sematrix.valid), "matrix")
    expect_equal(class(glmerSLMA.res$SLMA.pooled.ests.matrix), "matrix")
    expect_length(glmerSLMA.res$output.summary, 5)
    expect_equal(class(glmerSLMA.res$output.summary$input.beta.matrix.for.SLMA), "matrix")
    expect_equal(class(glmerSLMA.res$output.summary$input.se.matrix.for.SLMA), "matrix")
    expect_length(glmerSLMA.res$output.summary$study1, 20)
    expect_equal(class(glmerSLMA.res$output.summary$study1$coefficients), "matrix")
    expect_equal(as.numeric(glmerSLMA.res$output.summary$study1$AICtab[1]), 474.7665, tolerance = 0.001)
    expect_equal(as.numeric(glmerSLMA.res$output.summary$study1$devcomp$dims[1]), 1050)
    expect_equal(as.numeric(glmerSLMA.res$output.summary$study1$ngrps[2]), 8)
})

#
# Done
#

disconnect.studies.dataset.cluster()
