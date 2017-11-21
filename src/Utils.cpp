#include "Utils.h"


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                          REGRESSION                                **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

RegrResults regression(std::vector <double> x, std::vector <double> y)
{
    double sx, sx2, sy, sy2, sxy, t1, t2, xmn, ymn;
    RegrResults regr_results;

    sx = 0.0; sx2 = 0.0;
    sy = 0.0; sy2 = 0.0; sxy = 0.0;
    int count = 0, n = x.size ();
    for (int i=0; i<n; i++)
    {
        if (x [i] < INFINITE_DOUBLE && y [i] < INFINITE_DOUBLE)
        {
            count++;
            sx += x [i];
            sx2 += x [i] * x [i];
            sy += y [i];
            sy2 += y [i] * y [i];
            sxy += x [i] * y [i];
        }
    }

    xmn = sx / (double) count;
    ymn = sy / (double) count;

    if (count > 0) {
        t1 = (sxy - sx * sy / (double) count);
        t2 = (sx2 - sx * sx / (double) count) * (sy2 - sy * sy / (double) count);
        regr_results.cov = t1 / (double) count;
        regr_results.r2 = t1 / sqrt(t2); 
        regr_results.slope = t1 / (sx2 - sx * sx / (double) count); 
        regr_results.intercept = sy / (double) count -
            regr_results.slope * sx / (double) count; 

        regr_results.r2 = regr_results.r2 * regr_results.r2;
        if (regr_results.slope < 0.0) { regr_results.r2 = -regr_results.r2;     }

        // Then calculate SS and tval
        sy2 = 0.0; sx2 = 0.0; count = 0;
        regr_results.SS = 0.0;
        for (int i=0; i<n; i++) {
            //if (!isnan (x [i]) && !isnan (y [i])) {
                    count++;
                    t1 = regr_results.slope * x [i] + regr_results.intercept;
                    regr_results.SS += (y [i] - t1) * (y [i] - t1);
                    sx2 += (x [i] - xmn) * (x [i] - xmn);
                    sy2 += (y [i] - ymn) * (y [i] - ymn);
            //}
        } // end for i
        if (count > 0) { // tval calculation
            regr_results.SS = regr_results.SS / (double) count;
            regr_results.tval = sqrt(sy2 / ((double) count - 2.0)) / sqrt(sx2);
            regr_results.tval = regr_results.slope / regr_results.tval;
        } else {
            regr_results.SS = regr_results.tval = NAN;
        }
    } // end if count > 0
    else {
        regr_results.r2 = regr_results.slope = regr_results.intercept =
            regr_results.cov = regr_results.SS = regr_results.tval = NAN;
    }
    return regr_results;
}
// end function regression


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                              CALCMI                                **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

double calcMI (std::vector <double> x, std::vector <double> y)
{
    double mi = 0.0, sum = 0.0, xsum = 0.0, ysum = 0.0;

    assert (x.size () == y.size ());
    int n = x.size ();

    for (int i=0; i<n; i++)
        sum += x [i] + y [i];
    for (int i=0; i<n; i++)
    {
        x [i] = x [i] / sum;
        y [i] = y [i] / sum;
    }

    std::vector <double> colSums, xnull, ynull;
    colSums.reserve (n);
    xnull.reserve (n);
    ynull.reserve (n);

    for (int i=0; i<n; i++)
    {
        xsum += x [i];
        ysum += y [i];
        colSums [i] = x [i] + y [i];
    }

    for (int i=0; i<n; i++)
    {
        xnull [i] = colSums [i] * xsum;
        ynull [i] = colSums [i] * ysum;
    }

    for (int i=0; i<n; i++)
    {
        if (x [i] > 0.0)
            x [i] = x [i] * log2 (x [i] / xnull [i]);
        if (y [i] > 0.0)
            y [i] = y [i] * log2 (y [i] / ynull [i]);
        mi += x [i] + y [i];
    }

    colSums.resize (0);
    xnull.resize (0);
    ynull.resize (0);

    return (mi);
}
