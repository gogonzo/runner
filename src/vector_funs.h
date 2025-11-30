namespace aggr
{
  int calc_streak_l(Rcpp::LogicalVector const &x, int u, int l, bool na_rm)
  {
    int uu = u;
    int cur_streak{0};

    if (na_rm)
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::LogicalVector::is_na(x(j)))
          continue;
        if (Rcpp::LogicalVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }
    else
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::LogicalVector::is_na(x(j)))
          return NA_INTEGER;
        if (Rcpp::LogicalVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }

    if (cur_streak == 0)
      return NA_INTEGER;
    return cur_streak;
  }
  int calc_streak_i(Rcpp::IntegerVector const &x, int u, int l, bool na_rm)
  {
    int uu = u;
    int cur_streak{0};

    if (na_rm)
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::IntegerVector::is_na(x(j)))
          continue;
        if (Rcpp::IntegerVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }
    else
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::IntegerVector::is_na(x(j)))
          return NA_INTEGER;
        if (Rcpp::IntegerVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }

    if (cur_streak == 0)
      return NA_INTEGER;
    return cur_streak;
  }
  int calc_streak_n(Rcpp::NumericVector const &x, int u, int l, bool na_rm)
  {
    int uu = u;
    int cur_streak{0};

    if (na_rm)
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::NumericVector::is_na(x(j)))
          continue;
        if (Rcpp::NumericVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }
    else
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::NumericVector::is_na(x(j)))
          return NA_INTEGER;
        if (Rcpp::NumericVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }

    if (cur_streak == 0)
      return NA_INTEGER;
    return cur_streak;
  }
  int calc_streak_s(Rcpp::StringVector const &x, int u, int l, bool na_rm)
  {
    int uu = u;
    int cur_streak{0};

    if (na_rm)
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::StringVector::is_na(x(j)))
          continue;
        if (Rcpp::StringVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }
    else
    {
      for (int j = u; j >= l; --j)
      {
        if (Rcpp::StringVector::is_na(x(j)))
          return NA_INTEGER;
        if (Rcpp::StringVector::is_na(x(uu)))
          uu = j;
        if (x(j) == x(uu))
        {
          cur_streak += 1L;
        }
        else
        {
          break;
        }
      }
    }

    if (cur_streak == 0)
      return NA_INTEGER;
    return cur_streak;
  }

  double calc_max(Rcpp::NumericVector const &x, int u, int l, bool na_rm)
  {
    double cur_max = NA_REAL;

    if (na_rm)
    {
      for (int i = l; i <= u; ++i)
      {
        if (x(i) > cur_max or Rcpp::NumericVector::is_na(cur_max))
        {
          cur_max = x(i);
        }
      }
    }
    else
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          cur_max = NA_REAL;
          return cur_max;
        }
        if (Rcpp::NumericVector::is_na(cur_max) or x(i) > cur_max)
        {
          cur_max = x(i);
        }
      }
    }
    return cur_max;
  }
  double calc_min(Rcpp::NumericVector const &x, int u, int l, bool na_rm)
  {
    double cur_min = NA_REAL;

    if (na_rm)
    {
      for (int i = l; i <= u; ++i)
      {
        if (x(i) < cur_min or Rcpp::NumericVector::is_na(cur_min))
        {
          cur_min = x(i);
        }
      }
    }
    else
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          cur_min = NA_REAL;
          return cur_min;
        }
        if (Rcpp::NumericVector::is_na(cur_min) or x(i) < cur_min)
        {
          cur_min = x(i);
        }
      }
    }
    return cur_min;
  }
  double calc_sum(Rcpp::NumericVector const &x, int u, int l, bool na_rm)
  {
    double cur_sum = NA_REAL;
    if (na_rm)
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum = x(i);
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum += x(i);
        }
      }
    }
    else
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          return NA_REAL;
        }
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum = x(i);
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum += x(i);
        }
      }
    }
    return cur_sum;
  }
  double calc_mean(Rcpp::NumericVector const &x, int u, int l, bool na_rm)
  {
    double cur_sum = NA_REAL;
    int nonna = 0;

    if (na_rm)
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          nonna += 1;
          cur_sum = x(i);
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          nonna += 1;
          cur_sum += x(i);
        }
      }
    }
    else
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          return NA_REAL;
        }
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          nonna += 1;
          cur_sum = x(i);
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          nonna += 1;
          cur_sum += x(i);
        }
      }
    }
    return cur_sum / nonna;
  }
  int calc_whichf(Rcpp::LogicalVector const &x, int u, int l, bool na_rm)
  {

    if (na_rm)
    {
      for (int i = l; i <= u; ++i)
      {
        if (x(i) == TRUE)
        {
          return i + 1;
        }
      }
    }
    else
    {
      for (int i = l; i <= u; ++i)
      {
        if (Rcpp::LogicalVector::is_na(x(i)))
        {
          return NA_INTEGER;
        }
        else if (x(i) == TRUE)
        {
          return i + 1;
        }
      }
    }

    return NA_INTEGER;
  }
  int calc_whichl(Rcpp::LogicalVector const &x, int u, int l, bool na_rm)
  {
    if (na_rm)
    {
      for (int i = u; i >= l; --i)
      {
        if (x(i) == TRUE)
        {
          return i + 1;
        }
      }
    }
    else
    {
      for (int i = u; i >= l; --i)
      {
        if (Rcpp::LogicalVector::is_na(x(i)))
        {
          return NA_INTEGER;
        }
        else if (x(i) == TRUE)
        {
          return i + 1;
        }
      }
    }

    return NA_INTEGER;
  }

  template <int ITYPE>
  Rcpp::IntegerVector cumstreak(Rcpp::Vector<ITYPE> const &x, int lag, bool na_rm)
  {
    int n = x.size();
    Rcpp::IntegerVector res(n);
    int cur_streak = 0, l = 0;

    for (int i = 0; i < n; i++)
    {
      if (Rcpp::Vector<ITYPE>::is_na(x(i)))
      {
        if (!na_rm)
        {
          cur_streak = 0;
          if (i + lag >= 0 && i + lag < n)
          {
            res(i + lag) = NA_INTEGER;
            continue;
          }
        }
      }
      else if (x(i) == x(l))
      {
        cur_streak += 1;
      }
      else
      {
        cur_streak = 1;
        l = i;
      }

      if ((i + lag >= 0) && (i + lag < n))
      {
        res(i + lag) = cur_streak;
      }
    }

    if (lag > 0)
    {
      std::fill(res.begin(), res.end() - n + lag, NA_INTEGER);
    }
    else if (lag < 0)
    {
      std::fill(res.end() + lag, res.end(), NA_INTEGER);
    }

    return res;
  }
  Rcpp::NumericVector cummax(Rcpp::NumericVector const &x, bool na_rm)
  {
    int n = x.size();
    Rcpp::NumericVector res(n);
    double cur_max = NA_REAL;

    if (na_rm)
    {
      for (int i = 0; i < n; ++i)
      {
        if (x(i) > cur_max or Rcpp::NumericVector::is_na(cur_max))
        {
          cur_max = x(i);
        }
        res(i) = cur_max;
      }
    }
    else
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          std::fill(res.begin() + i, res.end(), NA_REAL);
          return res;
        }
        if (Rcpp::NumericVector::is_na(cur_max) or x(i) > cur_max)
        {
          cur_max = x(i);
        }
        res(i) = cur_max;
      }
    }
    return res;
  }
  Rcpp::NumericVector cummin(Rcpp::NumericVector const &x, bool na_rm)
  {
    int n = x.size();
    Rcpp::NumericVector res(n);
    double cur_max = NA_REAL;

    if (na_rm)
    {
      for (int i = 0; i < n; ++i)
      {
        if (x(i) < cur_max or Rcpp::NumericVector::is_na(cur_max))
        {
          cur_max = x(i);
        }
        res(i) = cur_max;
      }
    }
    else
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          std::fill(res.begin() + i, res.end(), NA_REAL);
          return res;
        }
        if (Rcpp::NumericVector::is_na(cur_max) or x(i) < cur_max)
        {
          cur_max = x(i);
        }
        res(i) = cur_max;
      }
    }
    return res;
  }
  Rcpp::NumericVector cumsum(Rcpp::NumericVector const &x, bool na_rm)
  {
    int n = x.size();
    Rcpp::NumericVector res(n);
    double cur_sum = NA_REAL;

    if (na_rm)
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum = x(i);
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum += x(i);
        }
        res(i) = cur_sum;
      }
    }
    else
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          std::fill(res.begin() + i, res.end(), NA_REAL);
          return res;
        }
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum = x(i);
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum += x(i);
        }
        res(i) = cur_sum;
      }
    }
    return res;
  }
  Rcpp::NumericVector cummean(Rcpp::NumericVector const &x, bool na_rm)
  {
    int n = x.size();
    Rcpp::NumericVector res(n);
    double cur_sum = NA_REAL;
    double nonna = 0;

    if (na_rm)
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum = x(i);
          nonna += 1;
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum += x(i);
          nonna += 1;
        }
        res(i) = cur_sum / nonna;
      }
    }
    else
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::NumericVector::is_na(x(i)))
        {
          std::fill(res.begin() + i, res.end(), NA_REAL);
          return res;
        }
        if (Rcpp::NumericVector::is_na(cur_sum) && !Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum = x(i);
          nonna += 1;
        }
        else if (!Rcpp::NumericVector::is_na(x(i)))
        {
          cur_sum += x(i);
          nonna += 1;
        }
        res(i) = cur_sum / nonna;
      }
    }
    return res;
  }
  Rcpp::IntegerVector cumwhichf(Rcpp::LogicalVector const &x, bool na_rm)
  {
    int n = x.size();
    Rcpp::IntegerVector res(n);
    double whicht = NA_INTEGER;

    if (na_rm)
    {
      for (int i = 0; i < n; ++i)
      {
        if (x(i) == TRUE)
        {
          std::fill(res.begin() + i, res.end(), i + 1);
          return res;
        }
        res(i) = whicht;
      }
    }
    else
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::LogicalVector::is_na(x(i)))
        {
          std::fill(res.begin() + i, res.end(), NA_INTEGER);
          return res;
        }
        else if (x(i) == TRUE)
        {
          std::fill(res.begin() + i, res.end(), i + 1);
          return res;
        }
        res(i) = whicht;
      }
    }
    return res;
  }
  Rcpp::IntegerVector cumwhichl(Rcpp::LogicalVector const &x, bool na_rm)
  {
    int n = x.size();
    Rcpp::IntegerVector res(n);
    double whicht = NA_INTEGER;

    if (na_rm)
    {
      for (int i = 0; i < n; ++i)
      {
        if (x(i) == TRUE)
        {
          whicht = i + 1;
        }
        res(i) = whicht;
      }
    }
    else
    {
      for (int i = 0; i < n; ++i)
      {
        if (Rcpp::LogicalVector::is_na(x(i)))
        {
          whicht = NA_INTEGER;
        }
        else if (x(i) == TRUE)
        {
          whicht = i + 1;
        }
        res(i) = whicht;
      }
    }
    return res;
  }
}
