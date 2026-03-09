#pragma once

#include <algorithm>
#include <cmath>
#include <initializer_list>
#include <limits>
#include <numeric>
#include <ostream>
#include <fstream>
#include <iomanip>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace pycpp {

struct ArgParser {};

template <typename T>
class Array1D {
public:
    Array1D() = default;
    explicit Array1D(std::size_t n) : data_(n) {}
    Array1D(std::size_t n, const T& value) : data_(n, value) {}
    Array1D(std::initializer_list<T> init) : data_(init) {}
    explicit Array1D(std::vector<T> data) : data_(std::move(data)) {}

    std::size_t size() const { return data_.size(); }
    bool empty() const { return data_.empty(); }

    T& operator[](std::size_t index) { return data_[index]; }
    const T& operator[](std::size_t index) const { return data_[index]; }

    T& at(std::size_t index) { return data_.at(index); }
    const T& at(std::size_t index) const { return data_.at(index); }

    void push_back(const T& value) { data_.push_back(value); }

    auto begin() { return data_.begin(); }
    auto end() { return data_.end(); }
    auto begin() const { return data_.begin(); }
    auto end() const { return data_.end(); }

    Array1D<T> copy() const { return Array1D<T>(data_); }
    Array1D<T> ravel() const { return Array1D<T>(data_); }

    T sum() const {
        return std::accumulate(data_.begin(), data_.end(), T{});
    }

    const std::vector<T>& raw() const { return data_; }
    std::vector<T>& raw() { return data_; }

private:
    std::vector<T> data_;
};

template <typename T>
class Array2D {
public:
    Array2D() = default;
    Array2D(std::size_t rows, std::size_t cols) : rows_(rows), cols_(cols), data_(rows * cols) {}
    Array2D(std::size_t rows, std::size_t cols, const T& value) : rows_(rows), cols_(cols), data_(rows * cols, value) {}
    Array2D(std::initializer_list<std::initializer_list<T>> init) {
        rows_ = init.size();
        cols_ = rows_ == 0 ? 0 : init.begin()->size();
        data_.reserve(rows_ * cols_);
        for (const auto& row : init) {
            if (row.size() != cols_) {
                throw std::runtime_error("ragged Array2D initializer");
            }
            data_.insert(data_.end(), row.begin(), row.end());
        }
    }

    std::size_t rows() const { return rows_; }
    std::size_t cols() const { return cols_; }
    std::size_t size() const { return data_.size(); }
    bool empty() const { return data_.empty(); }

    T& operator()(std::size_t row, std::size_t col) { return data_[row * cols_ + col]; }
    const T& operator()(std::size_t row, std::size_t col) const { return data_[row * cols_ + col]; }

    Array2D<T> copy() const { return *this; }
    Array1D<T> ravel() const { return Array1D<T>(data_); }

private:
    std::size_t rows_ = 0;
    std::size_t cols_ = 0;
    std::vector<T> data_;
};

template <typename T>
Array1D<T> array(std::initializer_list<T> init) {
    return Array1D<T>(init);
}

template <typename T>
Array1D<T> asarray(const Array1D<T>& value) {
    return value;
}

template <typename T>
Array2D<T> asarray(const Array2D<T>& value) {
    return value;
}

template <typename T>
Array1D<T> asarray(const std::vector<T>& value) {
    return Array1D<T>(value);
}

template <typename T>
Array1D<T> full(std::size_t n, const T& value) {
    return Array1D<T>(n, value);
}

template <typename T>
Array1D<T> empty(std::size_t n) {
    return Array1D<T>(n);
}

template <typename T>
Array2D<T> full(std::size_t rows, std::size_t cols, const T& value) {
    return Array2D<T>(rows, cols, value);
}

template <typename T>
Array2D<T> empty(std::size_t rows, std::size_t cols) {
    return Array2D<T>(rows, cols);
}

template <typename T>
Array2D<T> eye(std::size_t n) {
    Array2D<T> out(n, n, T{});
    for (std::size_t i = 0; i < n; ++i) {
        out(i, i) = static_cast<T>(1);
    }
    return out;
}

template <typename T>
Array2D<T> scalar_matrix(const T& value) {
    Array2D<T> out(1, 1);
    out(0, 0) = value;
    return out;
}

template <typename T>
T to_scalar(const Array2D<T>& value) {
    if (value.rows() != 1 || value.cols() != 1) {
        throw std::runtime_error("to_scalar expects a 1x1 matrix");
    }
    return value(0, 0);
}

template <typename T>
T sum(const Array1D<T>& value) {
    return value.sum();
}

template <typename T>
T sum(const Array2D<T>& value) {
    T out{};
    for (std::size_t i = 0; i < value.rows(); ++i) {
        for (std::size_t j = 0; j < value.cols(); ++j) {
            out += value(i, j);
        }
    }
    return out;
}

template <typename T>
T var(const Array1D<T>& value) {
    if (value.empty()) {
        throw std::runtime_error("var requires a non-empty array");
    }
    const T mean = value.sum() / static_cast<T>(value.size());
    T accum = T{};
    for (const auto& item : value) {
        const T diff = item - mean;
        accum += diff * diff;
    }
    return accum / static_cast<T>(value.size());
}

template <typename T>
Array2D<T> row_vector(const Array1D<T>& value) {
    Array2D<T> out(1, value.size());
    for (std::size_t j = 0; j < value.size(); ++j) {
        out(0, j) = value[j];
    }
    return out;
}

template <typename T>
Array2D<T> transpose(const Array2D<T>& value) {
    Array2D<T> out(value.cols(), value.rows());
    for (std::size_t i = 0; i < value.rows(); ++i) {
        for (std::size_t j = 0; j < value.cols(); ++j) {
            out(j, i) = value(i, j);
        }
    }
    return out;
}

template <typename T>
Array2D<T> col_vector(const Array1D<T>& value) {
    Array2D<T> out(value.size(), 1);
    for (std::size_t i = 0; i < value.size(); ++i) {
        out(i, 0) = value[i];
    }
    return out;
}

template <typename T, typename F>
Array1D<T> unary_1d(const Array1D<T>& value, F&& fn) {
    Array1D<T> out(value.size());
    for (std::size_t i = 0; i < value.size(); ++i) out[i] = fn(value[i]);
    return out;
}

template <typename T, typename F>
Array2D<T> unary_2d(const Array2D<T>& value, F&& fn) {
    Array2D<T> out(value.rows(), value.cols());
    for (std::size_t i = 0; i < value.rows(); ++i) {
        for (std::size_t j = 0; j < value.cols(); ++j) out(i, j) = fn(value(i, j));
    }
    return out;
}

template <typename T, typename F>
Array1D<T> binary_1d(const Array1D<T>& left, const Array1D<T>& right, F&& fn) {
    if (left.size() != right.size()) throw std::runtime_error("1D array sizes must match");
    Array1D<T> out(left.size());
    for (std::size_t i = 0; i < left.size(); ++i) out[i] = fn(left[i], right[i]);
    return out;
}

template <typename T, typename F>
Array2D<T> binary_2d(const Array2D<T>& left, const Array2D<T>& right, F&& fn) {
    const std::size_t rows =
        left.rows() == right.rows() ? left.rows() :
        (left.rows() == 1 ? right.rows() : (right.rows() == 1 ? left.rows() : 0));
    const std::size_t cols =
        left.cols() == right.cols() ? left.cols() :
        (left.cols() == 1 ? right.cols() : (right.cols() == 1 ? left.cols() : 0));
    if (rows == 0 || cols == 0) throw std::runtime_error("2D broadcast shape mismatch");
    Array2D<T> out(rows, cols);
    for (std::size_t i = 0; i < rows; ++i) {
        const std::size_t li = left.rows() == 1 ? 0 : i;
        const std::size_t ri = right.rows() == 1 ? 0 : i;
        for (std::size_t j = 0; j < cols; ++j) {
            const std::size_t lj = left.cols() == 1 ? 0 : j;
            const std::size_t rj = right.cols() == 1 ? 0 : j;
            out(i, j) = fn(left(li, lj), right(ri, rj));
        }
    }
    return out;
}

template <typename T> Array1D<T> operator+(const Array1D<T>& a, const Array1D<T>& b) { return binary_1d(a, b, [](T x, T y){ return x + y; }); }
template <typename T> Array1D<T> operator-(const Array1D<T>& a, const Array1D<T>& b) { return binary_1d(a, b, [](T x, T y){ return x - y; }); }
template <typename T> Array1D<T> operator*(const Array1D<T>& a, const Array1D<T>& b) { return binary_1d(a, b, [](T x, T y){ return x * y; }); }
template <typename T> Array1D<T> operator/(const Array1D<T>& a, const Array1D<T>& b) { return binary_1d(a, b, [](T x, T y){ return x / y; }); }
template <typename T, typename U> Array1D<T> operator+(const Array1D<T>& a, const U& s) { return unary_1d(a, [&](T x){ return x + static_cast<T>(s); }); }
template <typename T, typename U> Array1D<T> operator-(const Array1D<T>& a, const U& s) { return unary_1d(a, [&](T x){ return x - static_cast<T>(s); }); }
template <typename T, typename U> Array1D<T> operator*(const Array1D<T>& a, const U& s) { return unary_1d(a, [&](T x){ return x * static_cast<T>(s); }); }
template <typename T, typename U> Array1D<T> operator/(const Array1D<T>& a, const U& s) { return unary_1d(a, [&](T x){ return x / static_cast<T>(s); }); }
template <typename T, typename U> Array1D<T> operator+(const U& s, const Array1D<T>& a) { return a + s; }
template <typename T, typename U> Array1D<T> operator-(const U& s, const Array1D<T>& a) { return unary_1d(a, [&](T x){ return static_cast<T>(s) - x; }); }
template <typename T, typename U> Array1D<T> operator*(const U& s, const Array1D<T>& a) { return a * s; }
template <typename T, typename U> Array1D<T> operator/(const U& s, const Array1D<T>& a) { return unary_1d(a, [&](T x){ return static_cast<T>(s) / x; }); }

template <typename T> Array2D<T> operator+(const Array2D<T>& a, const Array2D<T>& b) { return binary_2d(a, b, [](T x, T y){ return x + y; }); }
template <typename T> Array2D<T> operator-(const Array2D<T>& a, const Array2D<T>& b) { return binary_2d(a, b, [](T x, T y){ return x - y; }); }
template <typename T> Array2D<T> operator*(const Array2D<T>& a, const Array2D<T>& b) { return binary_2d(a, b, [](T x, T y){ return x * y; }); }
template <typename T> Array2D<T> operator/(const Array2D<T>& a, const Array2D<T>& b) { return binary_2d(a, b, [](T x, T y){ return x / y; }); }
template <typename T, typename U> Array2D<T> operator+(const Array2D<T>& a, const U& s) { return unary_2d(a, [&](T x){ return x + static_cast<T>(s); }); }
template <typename T, typename U> Array2D<T> operator-(const Array2D<T>& a, const U& s) { return unary_2d(a, [&](T x){ return x - static_cast<T>(s); }); }
template <typename T, typename U> Array2D<T> operator*(const Array2D<T>& a, const U& s) { return unary_2d(a, [&](T x){ return x * static_cast<T>(s); }); }
template <typename T, typename U> Array2D<T> operator/(const Array2D<T>& a, const U& s) { return unary_2d(a, [&](T x){ return x / static_cast<T>(s); }); }
template <typename T, typename U> Array2D<T> operator+(const U& s, const Array2D<T>& a) { return a + s; }
template <typename T, typename U> Array2D<T> operator-(const U& s, const Array2D<T>& a) { return unary_2d(a, [&](T x){ return static_cast<T>(s) - x; }); }
template <typename T, typename U> Array2D<T> operator*(const U& s, const Array2D<T>& a) { return a * s; }
template <typename T, typename U> Array2D<T> operator/(const U& s, const Array2D<T>& a) { return unary_2d(a, [&](T x){ return static_cast<T>(s) / x; }); }
template <typename T> Array2D<T> operator+(const Array2D<T>& a, const Array1D<T>& b) { return a + row_vector(b); }
template <typename T> Array2D<T> operator-(const Array2D<T>& a, const Array1D<T>& b) { return a - row_vector(b); }
template <typename T> Array2D<T> operator*(const Array2D<T>& a, const Array1D<T>& b) { return a * row_vector(b); }
template <typename T> Array2D<T> operator/(const Array2D<T>& a, const Array1D<T>& b) { return a / row_vector(b); }

template <typename T>
Array2D<T> matmul(const Array2D<T>& left, const Array2D<T>& right) {
    if (left.cols() != right.rows()) {
        throw std::runtime_error("matmul dimension mismatch");
    }
    Array2D<T> out(left.rows(), right.cols(), T{});
    for (std::size_t i = 0; i < left.rows(); ++i) {
        for (std::size_t k = 0; k < left.cols(); ++k) {
            for (std::size_t j = 0; j < right.cols(); ++j) {
                out(i, j) += left(i, k) * right(k, j);
            }
        }
    }
    return out;
}

template <typename T>
Array1D<T> matmul(const Array2D<T>& left, const Array1D<T>& right) {
    if (left.cols() != right.size()) {
        throw std::runtime_error("matmul dimension mismatch");
    }
    Array1D<T> out(left.rows());
    for (std::size_t i = 0; i < left.rows(); ++i) {
        T sum{};
        for (std::size_t j = 0; j < left.cols(); ++j) {
            sum += left(i, j) * right[j];
        }
        out[i] = sum;
    }
    return out;
}

template <typename T>
Array2D<T> maximum(const Array2D<T>& left, const T& scalar) {
    Array2D<T> out(left.rows(), left.cols());
    for (std::size_t i = 0; i < left.rows(); ++i) {
        for (std::size_t j = 0; j < left.cols(); ++j) out(i, j) = std::max(left(i, j), scalar);
    }
    return out;
}

template <typename T>
Array1D<T> maximum(const Array1D<T>& left, const Array1D<T>& right) {
    if (left.size() != right.size()) {
        throw std::runtime_error("maximum requires arrays of equal size");
    }
    Array1D<T> out(left.size());
    for (std::size_t i = 0; i < left.size(); ++i) {
        out[i] = std::max(left[i], right[i]);
    }
    return out;
}

template <typename T>
Array1D<T> maximum(const Array1D<T>& left, const T& scalar) {
    Array1D<T> out(left.size());
    for (std::size_t i = 0; i < left.size(); ++i) {
        out[i] = std::max(left[i], scalar);
    }
    return out;
}

template <typename T>
Array1D<T> sqrt(const Array1D<T>& value) {
    Array1D<T> out(value.size());
    for (std::size_t i = 0; i < value.size(); ++i) {
        out[i] = static_cast<T>(std::sqrt(value[i]));
    }
    return out;
}

template <typename T>
Array2D<T> sqrt(const Array2D<T>& value) {
    return unary_2d(value, [](T x) { return static_cast<T>(std::sqrt(x)); });
}

template <typename T>
Array1D<T> log(const Array1D<T>& value) {
    Array1D<T> out(value.size());
    for (std::size_t i = 0; i < value.size(); ++i) {
        out[i] = static_cast<T>(std::log(value[i]));
    }
    return out;
}

template <typename T>
Array2D<T> log(const Array2D<T>& value) {
    return unary_2d(value, [](T x) { return static_cast<T>(std::log(x)); });
}

template <typename T>
Array1D<T> exp(const Array1D<T>& value) {
    Array1D<T> out(value.size());
    for (std::size_t i = 0; i < value.size(); ++i) {
        out[i] = static_cast<T>(std::exp(value[i]));
    }
    return out;
}

template <typename T>
Array2D<T> exp(const Array2D<T>& value) {
    return unary_2d(value, [](T x) { return static_cast<T>(std::exp(x)); });
}

template <typename T>
Array1D<T> pow(const Array1D<T>& value, double exponent) {
    return unary_1d(value, [&](T x) { return static_cast<T>(std::pow(x, exponent)); });
}

template <typename T>
Array2D<T> pow(const Array2D<T>& value, double exponent) {
    return unary_2d(value, [&](T x) { return static_cast<T>(std::pow(x, exponent)); });
}

template <typename T>
Array1D<T> sum_axis(const Array2D<T>& value, int axis) {
    if (axis == 0) {
        Array1D<T> out(value.cols(), T{});
        for (std::size_t j = 0; j < value.cols(); ++j) for (std::size_t i = 0; i < value.rows(); ++i) out[j] += value(i, j);
        return out;
    }
    if (axis == 1) {
        Array1D<T> out(value.rows(), T{});
        for (std::size_t i = 0; i < value.rows(); ++i) for (std::size_t j = 0; j < value.cols(); ++j) out[i] += value(i, j);
        return out;
    }
    throw std::runtime_error("sum_axis supports axis 0 or 1");
}

template <typename T>
Array2D<T> sum_axis_keepdims(const Array2D<T>& value, int axis) {
    if (axis == 0) return row_vector(sum_axis(value, 0));
    if (axis == 1) return col_vector(sum_axis(value, 1));
    throw std::runtime_error("sum_axis_keepdims supports axis 0 or 1");
}

template <typename T>
Array1D<T> max_axis(const Array2D<T>& value, int axis) {
    if (axis == 0) {
        Array1D<T> out(value.cols());
        for (std::size_t j = 0; j < value.cols(); ++j) {
            T best = value(0, j);
            for (std::size_t i = 1; i < value.rows(); ++i) best = std::max(best, value(i, j));
            out[j] = best;
        }
        return out;
    }
    if (axis == 1) {
        Array1D<T> out(value.rows());
        for (std::size_t i = 0; i < value.rows(); ++i) {
            T best = value(i, 0);
            for (std::size_t j = 1; j < value.cols(); ++j) best = std::max(best, value(i, j));
            out[i] = best;
        }
        return out;
    }
    throw std::runtime_error("max_axis supports axis 0 or 1");
}

template <typename T>
Array2D<T> max_axis_keepdims(const Array2D<T>& value, int axis) {
    if (axis == 0) return row_vector(max_axis(value, 0));
    if (axis == 1) return col_vector(max_axis(value, 1));
    throw std::runtime_error("max_axis_keepdims supports axis 0 or 1");
}

template <typename T>
Array1D<T> squeeze(const Array2D<T>& value, int axis) {
    if (axis == 0 && value.rows() == 1) {
        Array1D<T> out(value.cols());
        for (std::size_t j = 0; j < value.cols(); ++j) out[j] = value(0, j);
        return out;
    }
    if (axis == 1 && value.cols() == 1) {
        Array1D<T> out(value.rows());
        for (std::size_t i = 0; i < value.rows(); ++i) out[i] = value(i, 0);
        return out;
    }
    throw std::runtime_error("squeeze axis does not have size 1");
}

template <typename T>
Array2D<T> unsqueeze(const Array1D<T>& value, int axis) {
    if (axis == 0) return row_vector(value);
    if (axis == 1) return col_vector(value);
    throw std::runtime_error("unsqueeze supports axis 0 or 1");
}

template <typename T>
Array1D<int> argsort(const Array1D<T>& value) {
    std::vector<int> indices(value.size());
    std::iota(indices.begin(), indices.end(), 0);
    std::sort(indices.begin(), indices.end(), [&](int lhs, int rhs) {
        return value[static_cast<std::size_t>(lhs)] < value[static_cast<std::size_t>(rhs)];
    });
    return Array1D<int>(indices);
}

inline Array1D<int> choice(
    std::mt19937& rng,
    int k,
    std::size_t size,
    const Array1D<double>& probabilities
) {
    if (probabilities.size() != static_cast<std::size_t>(k)) {
        throw std::runtime_error("choice probabilities must have length k");
    }
    std::discrete_distribution<int> dist(probabilities.begin(), probabilities.end());
    Array1D<int> out(size);
    for (std::size_t i = 0; i < size; ++i) {
        out[i] = dist(rng);
    }
    return out;
}

template <typename T>
Array1D<T> take(const Array1D<T>& values, const Array1D<int>& indices) {
    Array1D<T> out(indices.size());
    for (std::size_t i = 0; i < indices.size(); ++i) {
        out[i] = values[static_cast<std::size_t>(indices[i])];
    }
    return out;
}

template <typename T>
Array1D<T> slice(const Array1D<T>& values, int start, int stop) {
    if (start < 0 || stop < start || static_cast<std::size_t>(stop) > values.size()) {
        throw std::runtime_error("slice bounds out of range");
    }
    Array1D<T> out(static_cast<std::size_t>(stop - start));
    for (int i = start; i < stop; ++i) out[static_cast<std::size_t>(i - start)] = values[static_cast<std::size_t>(i)];
    return out;
}

template <typename T>
Array1D<T> row(const Array2D<T>& values, int index) {
    if (index < 0 || static_cast<std::size_t>(index) >= values.rows()) {
        throw std::runtime_error("row index out of range");
    }
    Array1D<T> out(values.cols());
    for (std::size_t j = 0; j < values.cols(); ++j) {
        out[j] = values(static_cast<std::size_t>(index), j);
    }
    return out;
}

template <typename T>
void set_row(Array2D<T>& target, int index, const Array1D<T>& values) {
    if (index < 0 || static_cast<std::size_t>(index) >= target.rows() || values.size() != target.cols()) {
        throw std::runtime_error("set_row shape mismatch");
    }
    for (std::size_t j = 0; j < target.cols(); ++j) {
        target(static_cast<std::size_t>(index), j) = values[j];
    }
}

template <typename T>
Array1D<T> col(const Array2D<T>& values, int index) {
    if (index < 0 || static_cast<std::size_t>(index) >= values.cols()) {
        throw std::runtime_error("column index out of range");
    }
    Array1D<T> out(values.rows());
    for (std::size_t i = 0; i < values.rows(); ++i) {
        out[i] = values(i, static_cast<std::size_t>(index));
    }
    return out;
}

template <typename T>
void set_col(Array2D<T>& target, int index, const Array1D<T>& values) {
    if (index < 0 || static_cast<std::size_t>(index) >= target.cols() || values.size() != target.rows()) {
        throw std::runtime_error("set_col shape mismatch");
    }
    for (std::size_t i = 0; i < target.rows(); ++i) {
        target(i, static_cast<std::size_t>(index)) = values[i];
    }
}

template <typename T>
Array2D<T> take_rows(const Array2D<T>& values, const Array1D<int>& indices) {
    Array2D<T> out(indices.size(), values.cols());
    for (std::size_t i = 0; i < indices.size(); ++i) {
        const int src = indices[i];
        if (src < 0 || static_cast<std::size_t>(src) >= values.rows()) {
            throw std::runtime_error("row index out of range");
        }
        for (std::size_t j = 0; j < values.cols(); ++j) {
            out(i, j) = values(static_cast<std::size_t>(src), j);
        }
    }
    return out;
}

template <typename T>
std::vector<T> take_vector(const std::vector<T>& values, const Array1D<int>& indices) {
    std::vector<T> out;
    out.reserve(indices.size());
    for (std::size_t i = 0; i < indices.size(); ++i) {
        const int idx = indices[i];
        if (idx < 0 || static_cast<std::size_t>(idx) >= values.size()) {
            throw std::runtime_error("vector index out of range");
        }
        out.push_back(values[static_cast<std::size_t>(idx)]);
    }
    return out;
}

template <typename T>
void set_rows(Array2D<T>& target, const Array1D<int>& indices, const Array2D<T>& values) {
    if (indices.size() != values.rows() || target.cols() != values.cols()) {
        throw std::runtime_error("set_rows shape mismatch");
    }
    for (std::size_t i = 0; i < indices.size(); ++i) {
        const int dst = indices[i];
        if (dst < 0 || static_cast<std::size_t>(dst) >= target.rows()) {
            throw std::runtime_error("row index out of range");
        }
        for (std::size_t j = 0; j < target.cols(); ++j) {
            target(static_cast<std::size_t>(dst), j) = values(i, j);
        }
    }
}

template <typename T>
void add_diag(Array2D<T>& target, const T& value) {
    const std::size_t n = std::min(target.rows(), target.cols());
    for (std::size_t i = 0; i < n; ++i) {
        target(i, i) += value;
    }
}

template <typename T>
Array2D<T> reshape(const Array1D<T>& values, int rows, int cols) {
    if (rows < 0 || cols < 0 || static_cast<std::size_t>(rows * cols) != values.size()) {
        throw std::runtime_error("reshape size mismatch");
    }
    Array2D<T> out(static_cast<std::size_t>(rows), static_cast<std::size_t>(cols));
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            out(static_cast<std::size_t>(i), static_cast<std::size_t>(j)) =
                values[static_cast<std::size_t>(i * cols + j)];
        }
    }
    return out;
}

template <typename T>
Array1D<T> reshape(const Array2D<T>& values, int size) {
    if (size < 0 || static_cast<std::size_t>(size) != values.size()) {
        throw std::runtime_error("reshape size mismatch");
    }
    return values.ravel();
}

template <typename T>
Array1D<int> where_equal(const Array1D<T>& values, const T& needle) {
    std::vector<int> indices;
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (values[i] == needle) {
            indices.push_back(static_cast<int>(i));
        }
    }
    return Array1D<int>(std::move(indices));
}

inline Array1D<double> normal(
    std::mt19937& rng,
    const Array1D<double>& loc,
    const Array1D<double>& scale,
    std::size_t size
) {
    if (loc.size() != size || scale.size() != size) {
        throw std::runtime_error("normal expects loc and scale arrays matching requested size");
    }
    Array1D<double> out(size);
    for (std::size_t i = 0; i < size; ++i) {
        std::normal_distribution<double> dist(loc[i], scale[i]);
        out[i] = dist(rng);
    }
    return out;
}

inline Array1D<double> choice(
    std::mt19937& rng,
    const Array1D<double>& values,
    std::size_t size,
    bool replace
) {
    if (!replace && size > values.size()) {
        throw std::runtime_error("choice without replacement cannot exceed source size");
    }
    std::vector<int> indices(values.size());
    std::iota(indices.begin(), indices.end(), 0);
    std::shuffle(indices.begin(), indices.end(), rng);
    Array1D<double> out(size);
    if (replace) {
        std::uniform_int_distribution<int> dist(0, static_cast<int>(values.size()) - 1);
        for (std::size_t i = 0; i < size; ++i) out[i] = values[static_cast<std::size_t>(dist(rng))];
    } else {
        for (std::size_t i = 0; i < size; ++i) out[i] = values[static_cast<std::size_t>(indices[i])];
    }
    return out;
}

inline Array1D<int> choice(
    std::mt19937& rng,
    int n,
    std::size_t size,
    bool replace
) {
    if (!replace && size > static_cast<std::size_t>(n)) {
        throw std::runtime_error("choice without replacement cannot exceed source size");
    }
    Array1D<int> out(size);
    if (replace) {
        std::uniform_int_distribution<int> dist(0, n - 1);
        for (std::size_t i = 0; i < size; ++i) {
            out[i] = dist(rng);
        }
        return out;
    }
    std::vector<int> values(static_cast<std::size_t>(n));
    std::iota(values.begin(), values.end(), 0);
    std::shuffle(values.begin(), values.end(), rng);
    for (std::size_t i = 0; i < size; ++i) {
        out[i] = values[i];
    }
    return out;
}

inline Array2D<double> multivariate_normal(
    const Array1D<double>& mean,
    const Array2D<double>& cov,
    std::size_t size
) {
    const std::size_t d = mean.size();
    if (cov.rows() != d || cov.cols() != d) {
        throw std::runtime_error("multivariate_normal expects square covariance matching mean dimension");
    }

    Array2D<double> lower(d, d, 0.0);
    for (std::size_t i = 0; i < d; ++i) {
        for (std::size_t j = 0; j <= i; ++j) {
            double sum = cov(i, j);
            for (std::size_t k = 0; k < j; ++k) {
                sum -= lower(i, k) * lower(j, k);
            }
            if (i == j) {
                if (sum <= 0.0) {
                    throw std::runtime_error("covariance must be positive definite");
                }
                lower(i, j) = std::sqrt(sum);
            } else {
                lower(i, j) = sum / lower(j, j);
            }
        }
    }

    static thread_local std::mt19937 rng(std::random_device{}());
    std::normal_distribution<double> standard_normal(0.0, 1.0);
    Array2D<double> out(size, d);
    for (std::size_t row_index = 0; row_index < size; ++row_index) {
        std::vector<double> z(d);
        for (std::size_t j = 0; j < d; ++j) {
            z[j] = standard_normal(rng);
        }
        for (std::size_t i = 0; i < d; ++i) {
            double value = mean[i];
            for (std::size_t j = 0; j <= i; ++j) {
                value += lower(i, j) * z[j];
            }
            out(row_index, i) = value;
        }
    }
    return out;
}

inline std::tuple<double, double> slogdet(const Array2D<double>& input) {
    if (input.rows() != input.cols()) {
        throw std::runtime_error("slogdet expects a square matrix");
    }
    Array2D<double> a = input.copy();
    double sign = 1.0;
    double logabsdet = 0.0;
    const std::size_t n = a.rows();
    for (std::size_t i = 0; i < n; ++i) {
        std::size_t pivot = i;
        double best = std::abs(a(i, i));
        for (std::size_t r = i + 1; r < n; ++r) {
            const double candidate = std::abs(a(r, i));
            if (candidate > best) {
                best = candidate;
                pivot = r;
            }
        }
        if (best == 0.0) {
            return {0.0, -std::numeric_limits<double>::infinity()};
        }
        if (pivot != i) {
            sign = -sign;
            for (std::size_t c = 0; c < n; ++c) {
                std::swap(a(i, c), a(pivot, c));
            }
        }
        const double diag = a(i, i);
        sign *= diag > 0.0 ? 1.0 : -1.0;
        logabsdet += std::log(std::abs(diag));
        for (std::size_t r = i + 1; r < n; ++r) {
            const double factor = a(r, i) / diag;
            for (std::size_t c = i; c < n; ++c) {
                a(r, c) -= factor * a(i, c);
            }
        }
    }
    return {sign, logabsdet};
}

inline Array2D<double> inv(const Array2D<double>& input) {
    if (input.rows() != input.cols()) {
        throw std::runtime_error("inv expects a square matrix");
    }
    const std::size_t n = input.rows();
    Array2D<double> a = input.copy();
    Array2D<double> out = eye<double>(n);
    for (std::size_t i = 0; i < n; ++i) {
        std::size_t pivot = i;
        double best = std::abs(a(i, i));
        for (std::size_t r = i + 1; r < n; ++r) {
            const double candidate = std::abs(a(r, i));
            if (candidate > best) {
                best = candidate;
                pivot = r;
            }
        }
        if (best == 0.0) {
            throw std::runtime_error("matrix is singular");
        }
        if (pivot != i) {
            for (std::size_t c = 0; c < n; ++c) {
                std::swap(a(i, c), a(pivot, c));
                std::swap(out(i, c), out(pivot, c));
            }
        }
        const double diag = a(i, i);
        for (std::size_t c = 0; c < n; ++c) {
            a(i, c) /= diag;
            out(i, c) /= diag;
        }
        for (std::size_t r = 0; r < n; ++r) {
            if (r == i) {
                continue;
            }
            const double factor = a(r, i);
            for (std::size_t c = 0; c < n; ++c) {
                a(r, c) -= factor * a(i, c);
                out(r, c) -= factor * out(i, c);
            }
        }
    }
    return out;
}

inline Array1D<double> quad_form_rows(
    const Array2D<double>& left,
    const Array2D<double>& middle,
    const Array2D<double>& right
) {
    if (left.rows() != right.rows() || left.cols() != middle.rows() || middle.cols() != right.cols()) {
        throw std::runtime_error("quad_form_rows dimension mismatch");
    }
    Array1D<double> out(left.rows());
    for (std::size_t n = 0; n < left.rows(); ++n) {
        double value = 0.0;
        for (std::size_t i = 0; i < middle.rows(); ++i) {
            for (std::size_t j = 0; j < middle.cols(); ++j) {
                value += left(n, i) * middle(i, j) * right(n, j);
            }
        }
        out[n] = value;
    }
    return out;
}

inline Array2D<double> cov(const Array2D<double>& x, bool rowvar) {
    if (rowvar) {
        throw std::runtime_error("cov currently supports rowvar=false only");
    }
    if (x.rows() < 2) {
        throw std::runtime_error("cov requires at least two rows");
    }
    const std::size_t n = x.rows();
    const std::size_t d = x.cols();
    Array1D<double> mean(d, 0.0);
    for (std::size_t i = 0; i < n; ++i) {
        for (std::size_t j = 0; j < d; ++j) {
            mean[j] += x(i, j);
        }
    }
    for (std::size_t j = 0; j < d; ++j) {
        mean[j] /= static_cast<double>(n);
    }
    Array2D<double> out(d, d, 0.0);
    for (std::size_t i = 0; i < n; ++i) {
        for (std::size_t a = 0; a < d; ++a) {
            for (std::size_t b = 0; b < d; ++b) {
                out(a, b) += (x(i, a) - mean[a]) * (x(i, b) - mean[b]);
            }
        }
    }
    const double denom = static_cast<double>(n - 1);
    for (std::size_t a = 0; a < d; ++a) {
        for (std::size_t b = 0; b < d; ++b) {
            out(a, b) /= denom;
        }
    }
    return out;
}

template <typename T>
Array2D<double> column_stack(const Array1D<T>& first, const Array2D<double>& second) {
    if (first.size() != second.rows()) {
        throw std::runtime_error("column_stack row count mismatch");
    }
    Array2D<double> out(second.rows(), second.cols() + 1);
    for (std::size_t i = 0; i < second.rows(); ++i) {
        out(i, 0) = static_cast<double>(first[i]);
        for (std::size_t j = 0; j < second.cols(); ++j) {
            out(i, j + 1) = second(i, j);
        }
    }
    return out;
}

inline std::string format_value(double value, const std::string& spec) {
    std::ostringstream oss;
    if (!spec.empty() && spec[0] == '%' && spec.back() == 'f') {
        const auto dot = spec.find('.');
        int precision = 6;
        if (dot != std::string::npos) {
            precision = std::stoi(spec.substr(dot + 1, spec.size() - dot - 2));
        }
        oss << std::fixed << std::setprecision(precision) << value;
        return oss.str();
    }
    if (spec == "%d") {
        oss << static_cast<long long>(std::llround(value));
        return oss.str();
    }
    oss << value;
    return oss.str();
}

inline void savetxt(const std::string& path, const Array2D<double>& data, const std::vector<std::string>& formats) {
    std::ofstream out(path);
    if (!out) {
        throw std::runtime_error("could not open output file");
    }
    for (std::size_t i = 0; i < data.rows(); ++i) {
        for (std::size_t j = 0; j < data.cols(); ++j) {
            if (j > 0) {
                out << ' ';
            }
            const std::string spec = j < formats.size() ? formats[j] : "%.10f";
            out << format_value(data(i, j), spec);
        }
        out << '\n';
    }
}

inline void savetxt(const std::string& path, const Array2D<double>& data, const std::string& format) {
    savetxt(path, data, std::vector<std::string>(data.cols(), format));
}

inline Array2D<double> loadtxt(const std::string& path) {
    std::ifstream in(path);
    if (!in) {
        throw std::runtime_error("could not open input file");
    }
    std::vector<std::vector<double>> rows;
    std::string line;
    std::size_t cols = 0;
    while (std::getline(in, line)) {
        if (line.empty()) {
            continue;
        }
        std::istringstream iss(line);
        std::vector<double> row;
        double value = 0.0;
        while (iss >> value) {
            row.push_back(value);
        }
        if (row.empty()) {
            continue;
        }
        if (cols == 0) {
            cols = row.size();
        } else if (row.size() != cols) {
            throw std::runtime_error("loadtxt found inconsistent row lengths");
        }
        rows.push_back(std::move(row));
    }
    Array2D<double> out(rows.size(), cols, 0.0);
    for (std::size_t i = 0; i < rows.size(); ++i) {
        for (std::size_t j = 0; j < cols; ++j) {
            out(i, j) = rows[i][j];
        }
    }
    return out;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const Array1D<T>& value) {
    os << "[";
    for (std::size_t i = 0; i < value.size(); ++i) {
        if (i > 0) {
            os << " ";
        }
        os << value[i];
    }
    os << "]";
    return os;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const Array2D<T>& value) {
    os << "[";
    for (std::size_t i = 0; i < value.rows(); ++i) {
        if (i > 0) os << "; ";
        os << "[";
        for (std::size_t j = 0; j < value.cols(); ++j) {
            if (j > 0) os << " ";
            os << value(i, j);
        }
        os << "]";
    }
    os << "]";
    return os;
}

}  // namespace pycpp
