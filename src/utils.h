#ifndef utils_h
#define utils_h

#include <algorithm>

namespace utils
{
  // Boundaries of the lagged running window (positional, no idx) — O(1)
  Rcpp::IntegerVector window_ul(int i, int k, int lag, int n, bool na_pad, bool cum = false)
  {
    Rcpp::IntegerVector res(2);

    // exceptions
    if (na_pad)
    {
      if (cum)
      {
        if (i - lag >= n or lag > i)
          return Rcpp::IntegerVector(0);
      }
      else
      {
        if ((i - lag - k + 1) < 0 or (i - lag) >= n)
          return Rcpp::IntegerVector(0);
      }
      // |---------- [ ]    [ ] |----------
    }
    else
    {
      if (cum)
      {
        if (lag > i)
          return Rcpp::IntegerVector(0);
      }
      else
      {
        if (lag > i or (i - lag - k + 1) >= n)
          return Rcpp::IntegerVector(0);
      }
    }

    // upper bound
    if ((i - lag) >= n)
    {
      res(1) = n - 1;
    }
    else
    {
      res(1) = i - lag;
    }

    // lower bound
    if (cum || (i - k - lag + 1) <= 0)
    {
      res(0) = 0;
    }
    else
    {
      res(0) = i - k - lag + 1;
    }

    return res;
  }

  // Boundaries of the lagged running window based on indexes — O(log n) via binary search
  // indexes must be sorted ascending (enforced by checks::check_idx)
  Rcpp::IntegerVector window_ul_dl(Rcpp::IntegerVector const &indexes, int i, int k, int lag, int n, bool na_pad, bool cum = false)
  {
    if (na_pad)
    {
      if (cum)
      {
        if ((indexes(i) - lag > indexes(n - 1)) or (indexes(i) - lag < indexes(0)))
          return Rcpp::IntegerVector(0);
      }
      else
      {
        if (((indexes(i) - lag - k + 1) < indexes(0)) or ((indexes(i) - lag) > indexes(n - 1)))
          return Rcpp::IntegerVector(0);
      }
    }
    else
    {
      if (cum)
      {
        if (indexes(i) - lag < indexes(0))
          return Rcpp::IntegerVector(0);
      }
      else
      {
        if (((indexes(i) - lag) < indexes(0)) or ((indexes(i) - lag - k + 1) > indexes(n - 1)))
          return Rcpp::IntegerVector(0);
      }
    }

    Rcpp::IntegerVector idx_out(2);

    // cumulative ========================================================================
    if (cum)
    {
      int val_hi = indexes(i) - lag;

      if (lag >= 0)
      {
        // u = last position <= i where indexes(u) <= val_hi
        auto it = std::upper_bound(indexes.begin(), indexes.begin() + i + 1, val_hi);
        if (it == indexes.begin())
          return Rcpp::IntegerVector(0);
        idx_out(0) = 0;
        idx_out(1) = (int)(it - indexes.begin()) - 1;
        return idx_out;
      }
      else
      {
        // u = last position where indexes(u) <= val_hi (search from i forward)
        auto it = std::upper_bound(indexes.begin() + i, indexes.begin() + n, val_hi);
        int u = (int)(it - indexes.begin()) - 1;
        if (u < 0)
          return Rcpp::IntegerVector(0);
        idx_out(0) = 0;
        idx_out(1) = u;
        return idx_out;
      }
    }

    // non-cumulative ====================================================================
    int val_hi = indexes(i) - lag;
    int val_lo = indexes(i) - lag - k + 1;

    if (lag >= 0)
    {
      // u = last position <= i where indexes(u) <= val_hi
      auto it_u = std::upper_bound(indexes.begin(), indexes.begin() + i + 1, val_hi);
      if (it_u == indexes.begin())
        return Rcpp::IntegerVector(0);
      int u = (int)(it_u - indexes.begin()) - 1;

      // Check that u is within window width: indexes(u) >= val_lo
      if (indexes(u) < val_lo)
        return Rcpp::IntegerVector(0);

      // l = first position where indexes(l) >= val_lo
      auto it_l = std::lower_bound(indexes.begin(), indexes.begin() + u + 1, val_lo);
      int l = (int)(it_l - indexes.begin());

      idx_out(0) = l;
      idx_out(1) = u;
      return idx_out;
    }
    else
    {
      // lag < 0: window may span both sides of i or be entirely ahead
      if (-lag < k)
      {
        // Window spans both sides of i
        // l = first position in [0, i] where indexes(l) >= val_lo
        auto it_l = std::lower_bound(indexes.begin(), indexes.begin() + i + 1, val_lo);
        int l = (int)(it_l - indexes.begin());

        // u = last position in [i, n-1] where indexes(u) <= val_hi
        auto it_u = std::upper_bound(indexes.begin() + i, indexes.begin() + n, val_hi);
        int u = (int)(it_u - indexes.begin()) - 1;

        if (l > u)
          return Rcpp::IntegerVector(0);

        idx_out(0) = l;
        idx_out(1) = u;
        return idx_out;
      }
      else
      {
        // -lag >= k: window entirely ahead of i
        // l = first position in [i, n-1] where indexes(l) >= val_lo
        auto it_l = std::lower_bound(indexes.begin() + i, indexes.begin() + n, val_lo);
        int l = (int)(it_l - indexes.begin());

        if (l >= n || indexes(l) > val_hi)
          return Rcpp::IntegerVector(0);

        // u = last position where indexes(u) <= val_hi
        auto it_u = std::upper_bound(indexes.begin() + l, indexes.begin() + n, val_hi);
        int u = (int)(it_u - indexes.begin()) - 1;

        if (u < l)
          return Rcpp::IntegerVector(0);

        idx_out(0) = l;
        idx_out(1) = u;
        return idx_out;
      }
    }
    return Rcpp::IntegerVector(0);
  }

  // Boundaries of the lagged running window at arbitrary points — O(log n) via binary search
  Rcpp::IntegerVector window_ul_at(Rcpp::IntegerVector const &indexes, int at, int k, int lag, int n, bool na_pad, bool cum = false)
  {
    int li, ui;
    if (cum)
    {
      li = 0;
    }
    else
    {
      li = at - lag - k + 1;
    }
    ui = at - lag;

    if (na_pad)
    {
      if (cum)
      {
        if ((ui > indexes(n - 1)) or (ui < indexes(0)))
          return Rcpp::IntegerVector(0);
      }
      else
      {
        if ((li < indexes(0)) or (ui > indexes(n - 1)))
          return Rcpp::IntegerVector(0);
      }
    }
    else
    {
      if (cum)
      {
        if (ui < indexes(0))
          return Rcpp::IntegerVector(0);
      }
      else
      {
        if ((ui < indexes(0)) or (li > indexes(n - 1)))
          return Rcpp::IntegerVector(0);
      }
    }

    Rcpp::IntegerVector idx_out(2);

    // cumulative ========================================================================
    if (cum)
    {
      // u = last position where indexes(u) <= ui
      auto it = std::upper_bound(indexes.begin(), indexes.begin() + n, ui);
      if (it == indexes.begin())
        return Rcpp::IntegerVector(0);
      idx_out(0) = 0;
      idx_out(1) = (int)(it - indexes.begin()) - 1;
      return idx_out;
    }

    // non-cumulative ====================================================================
    if (lag >= 0)
    {
      // u = last position where indexes(u) <= ui
      auto it_u = std::upper_bound(indexes.begin(), indexes.begin() + n, ui);
      if (it_u == indexes.begin())
        return Rcpp::IntegerVector(0);
      int u = (int)(it_u - indexes.begin()) - 1;

      // Check that indexes(u) >= li
      if (indexes(u) < li)
        return Rcpp::IntegerVector(0);

      // l = first position where indexes(l) >= li
      auto it_l = std::lower_bound(indexes.begin(), indexes.begin() + u + 1, li);
      int l = (int)(it_l - indexes.begin());

      idx_out(0) = l;
      idx_out(1) = u;
      return idx_out;
    }
    else
    {
      // lag < 0
      // l = first position where indexes(l) >= li
      auto it_l = std::lower_bound(indexes.begin(), indexes.begin() + n, li);
      int l = (int)(it_l - indexes.begin());

      if (l >= n || indexes(l) > ui)
        return Rcpp::IntegerVector(0);

      // u = last position where indexes(u) <= ui
      auto it_u = std::upper_bound(indexes.begin() + l, indexes.begin() + n, ui);
      int u = (int)(it_u - indexes.begin()) - 1;

      if (u < l)
        return Rcpp::IntegerVector(0);

      idx_out(0) = l;
      idx_out(1) = u;
      return idx_out;
    }
    return Rcpp::IntegerVector(0);
  }
}

#endif
