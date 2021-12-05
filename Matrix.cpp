#include <iostream>
#include <vector>

using namespace std;

#pragma once
class Matrix
{
private:
    int n, m, size;
    vector<double> matrix;

    int _r(int n)
    {
        return (n - 1) / this->m;
    }
    int _c(int n)
    {
        return ((n + this->m - 1) % this->m);
    }
    int _l(int row, int column)
    {
        return (row - 1) * this->n + column - 1;
    }

public:
    Matrix(int l) : Matrix(l, l)
    {
    }
    Matrix(int n, int m)
    {
        if (n > 0)
            this->n = n;
        else
            this->n = 1;
        if (m > 0)
            this->m = m;
        else
            this->m = 1;
        size = this->n * this->m;
        for (int i = 0; i < this->size; i++)
        {
            this->matrix.push_back(0);
        }
    }
    ~Matrix()
    {
    }

    double set(int row, int column, double value)
    {
        if (this->n >= row && this->m >= column && row > 0 && column > 0)
            return this->matrix[_l(row, column)] = value;
        return 0;
    }
    double get(int row, int column)
    {
        if (this->n >= row && this->m >= column && row > 0 && column > 0)
            return this->matrix[_l(row, column)];
        return 0;
    }

    int Row()
    {
        return this->n;
    }
    int Column()
    {
        return this->m;
    }
    int Size()
    {
        return this->n * this->m;
    }

    double Determinant(Matrix matrix)
    {
        return matrix.Determinant();
    }
    double Determinant()
    {
        if (this->n != this->m)
            return 0;
        if (this->n == 1)
            return this->matrix[0];
        if (this->n == 2)
            return (this->matrix[0] * this->matrix[3]) - (this->matrix[1] * this->matrix[2]);
        double det = 0;
        for (int i = 1; i <= this->n; i++)
        {
            if (i % 2 == 1)
                det += this->matrix[i - 1] * Determinant(subMatrix(1, i));
            else
                det -= this->matrix[i - 1] * Determinant(subMatrix(1, i));
        }
        return det;
    }

    Matrix Inverse()
    {
        double det = Determinant();
        if (det == 0)
            return Matrix(1, 1);
        return (1 / det) * Adjugate();
    }

    // ماتریس کهاد
    Matrix Minor()
    {
        Matrix result(this->n, this->m);
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                result.matrix[result._l(i, j)] = Determinant(subMatrix(i, j));
        return result;
    }

    // ماتریس همساز
    Matrix Cofactor()
    {
        Matrix result(this->n, this->m);
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if ((i + j) % 2 == 0)
                    result.matrix[_l(i, j)] = this->matrix[_l(i, j)];
                else
                    result.matrix[_l(i, j)] = this->matrix[_l(i, j)] * -1;
        return result;
    }

    // ماتریس الحاقی
    Matrix Adjugate()
    {
        return Minor().Cofactor().Transpose();
    }

    // ماتریس الحاقی
    Matrix adj()
    {
        return Adjugate();
    }

    // ماتریس ترانهاده
    Matrix Transpose()
    {
        Matrix result(this->m, this->n);
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                result.matrix[result._l(j, i)] = this->matrix[_l(i, j)];
        return result;
    }

    Matrix subMatrix(int row, int column)
    {
        Matrix result(this->n - 1, this->m - 1);
        int a = 0;
        for (int i = 1; i <= this->n; i++)
        {
            for (int j = 1; j <= this->m; j++)
            {
                if (i != row && j != column)
                {
                    if (a == result.size)
                        break;
                    result.matrix[a] = this->matrix[_l(i, j)];
                    a++;
                }
            }
        }
        return result;
    }

    Matrix &operator=(double value)
    {
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (i == j)
                    this->matrix[_l(i, j)] = value;
                else
                    this->matrix[_l(i, j)] = 0;
        return *this;
    }
    Matrix &operator=(int value)
    {
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (i == j)
                    this->matrix[_l(i, j)] = value;
                else
                    this->matrix[_l(i, j)] = 0;
        return *this;
    }
    Matrix &operator=(std::initializer_list<double> list)
    {
        this->matrix.clear();
        if (this->size == list.size())
            this->matrix.assign(list);
        else if (this->size > list.size())
        {
            this->matrix.assign(list);
            for (int i = 0; i < this->size - list.size(); i++)
                this->matrix.push_back(0);
        }
        else if (this->size < list.size())
        {
            double *pointer = const_cast<double *>(list.begin());
            for (int i = 0; i < this->size; i++)
                this->matrix.push_back(*pointer++);
        }
        return *this;
    }
    Matrix &operator=(Matrix matrix)
    {
        this->matrix.clear();
        this->n = matrix.n;
        this->m = matrix.m;
        this->size = this->n * this->m;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                this->matrix.push_back(matrix.get(i, j));
        return *this;
    }
    Matrix &operator=(Matrix &matrix)
    {
        this->matrix.clear();
        this->n = matrix.n;
        this->m = matrix.m;
        this->size = this->n * this->m;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                this->matrix.push_back(matrix.get(i, j));
        return *this;
    }

    Matrix operator+(Matrix matrix)
    {
        if (matrix.n != this->n && matrix.m != this->m)
            return Matrix(1, 1) = 0;
        Matrix result(this->n, this->m);
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                result.set(i, j, this->matrix[_l(i, j)] + matrix.matrix[matrix._l(i, j)]);
        return result;
    }
    Matrix operator+=(Matrix matrix)
    {
        if (matrix.n != this->n && matrix.m != this->m)
            return Matrix(1, 1) = 0;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                this->matrix[_l(i, j)] += matrix.matrix[matrix._l(i, j)];
        return *this;
    }
    Matrix operator-(Matrix matrix)
    {
        if (matrix.n != this->n && matrix.m != this->m)
            return Matrix(1, 1) = 0;
        Matrix result(this->n, this->m);
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                result.set(i, j, this->matrix[_l(i, j)] - matrix.matrix[matrix._l(i, j)]);
        return result;
    }
    Matrix operator-=(Matrix matrix)
    {
        if (matrix.n != this->n && matrix.m != this->m)
            return Matrix(1, 1) = 0;
        for (int i = 10; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                this->matrix[_l(i, j)] -= matrix.matrix[matrix._l(i, j)];
        return *this;
    }
    Matrix operator*(Matrix matrix)
    {
        if (this->n != matrix.m || this->m != matrix.n)
            return Matrix(1, 1) = 0;
        Matrix result(this->n, this->m);
        for (int i = 1; i <= this->n; i++)
        {
            for (int j = 1; j <= this->m; j++)
            {
                double sum = 0;
                for (int k = 1; k <= this->m; k++)
                {
                    sum += this->matrix[_l(i, k)] * matrix.matrix[matrix._l(k, j)];
                }
                result.set(i, j, sum);
            }
        }
        return result;
    }
    Matrix operator*=(Matrix matrix)
    {
        if (this->n != matrix.m || this->m != matrix.n)
            return Matrix(1, 1) = 0;
        Matrix result(this->n, this->m);
        for (int i = 1; i <= this->n; i++)
        {
            for (int j = 1; j <= this->m; j++)
            {
                double sum = 0;
                for (int k = 1; k <= this->m; k++)
                {
                    sum += this->matrix[_l(i, k)] * matrix.matrix[matrix._l(k, j)];
                }
                this->matrix[_l(i, j)] = sum;
            }
        }
        return *this;
    }
    Matrix operator/(Matrix matrix)
    {
        if (this->n != matrix.m || this->m != matrix.n)
            return Matrix(1, 1) = 0;
        return *this * matrix.Inverse();
    }
    Matrix operator/=(Matrix matrix)
    {
        if (this->n != matrix.m || this->m != matrix.n)
            return Matrix(1, 1) = 0;
        return *this * matrix.Inverse();
    }

    friend inline Matrix operator*(double num, Matrix matrix)
    {
        Matrix result(matrix.n, matrix.m);
        for (int i = 0; i < matrix.size; i++)
        {
            result.matrix[i] = matrix.matrix[i] * num;
        }
        return result;
    }
    Matrix operator*(double num)
    {
        Matrix result(this->n, this->m);
        for (int i = 0; i < this->size; i++)
        {
            result.matrix[i] = this->matrix[i] * num;
        }
        return result;
    }
    friend inline Matrix operator*(int num, Matrix matrix)
    {
        Matrix result(matrix.n, matrix.m);
        for (int i = 0; i < matrix.size; i++)
        {
            result.matrix[i] = matrix.matrix[i] * num;
        }
        return result;
    }
    Matrix operator*(int num)
    {
        Matrix result(this->n, this->m);
        for (int i = 0; i < this->size; i++)
        {
            result.matrix[i] = this->matrix[i] * num;
        }
        return result;
    }

    friend ostream &operator<<(ostream &output, Matrix matrix)
    {
        for (int i = 1; i <= matrix.n; i++)
        {
            output << "| ";
            for (int j = 1; j < matrix.m; j++)
                output << matrix.matrix[matrix._l(i, j)] << "\t";
            output << matrix.matrix[matrix._l(i, matrix.m)] << " |\n";
        }
        return output;
    }
    friend istream &operator>>(istream &input, Matrix matrix)
    {
        for (int i = 1; i <= matrix.n; i++)
        {
            for (int j = 1; j <= matrix.m; j++)
            {
                cout << "(" << i << "," << j << "):";
                input >> matrix.matrix[matrix._l(i, j)];
            }
        }
        return input;
    }
    int operator[](int row)
    {
        return this->matrix[row];
    }

    // ماتریس سطری
    bool isRowMatrix()
    {
        return this->n == 1;
    }
    // ماتریس سطونی
    bool isColumnMatrix()
    {
        return this->m == 1;
    }
    // ماتریس مربعی
    bool isSquareMatrix()
    {
        return this->n == this->m;
    }
    // ماتریس قطری
    bool isDiagonalMatrix()
    {
        if (this->n != this->m)
            return false;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (i != j && this->matrix[_l(i, j)] != 0)
                    return false;
        return true;
    }
    // ماتریس پایین مثلثی
    bool isLowerTriangularMatrix()
    {
        if (this->n != this->m)
            return false;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (i < j && this->matrix[_l(i, j)] != 0)
                    return false;
        return true;
    }
    // ماتریس بالا مثلثی
    bool isUpperTriangularMatrix()
    {
        if (this->n != this->m)
            return false;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (i > j && this->matrix[_l(i, j)] != 0)
                    return false;
        return true;
    }
    // ماتریس هویت | ماتریس یکّه | ماتریس همانی
    bool isIdentityMatrix()
    {
        if (this->n != this->m)
            return false;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (i != j)
                {
                    if (this->matrix[_l(i, j)] != 0)
                        return false;
                }
                else if (this->matrix[_l(i, j)] != 1)
                    return false;
        return true;
    }
    // ماتریس متقارن
    bool isSymmetricMatrix()
    {
        if (this->n != this->m)
            return false;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (this->matrix[_l(i, j)] != this->matrix[_l(j, i)])
                    return false;
        return true;
    }
    // ماتریس پادمتقارن
    bool isSkewSymmetricMatrix()
    {
        if (this->n != this->m)
            return false;
        for (int i = 1; i <= this->n; i++)
            for (int j = 1; j <= this->m; j++)
                if (this->matrix[_l(i, j)] != this->matrix[_l(j, i)] * -1)
                    return false;
        return true;
    }
    // ماتریس معکوس پذیر
    bool isInvertibleMatrix()
    {
        if (this->n != this->m)
            return false;
        return Determinant() != 0;
    }
    // ماتریس متعامد
    // bool isOrthogonalMatrix() {}

    bool operator==(Matrix &matrix)
    {
        if (this->n != matrix.n || this->m != matrix.m)
            return false;
        for (int i = 0; i < this->size; i++)
            if (this->matrix[i] != matrix.matrix[i])
                return false;
        return true;
    }
    bool operator!=(Matrix &matrix)
    {
        if (this->n != matrix.n || this->m != matrix.m)
            return true;
        for (int i = 0; i < this->size; i++)
            if (this->matrix[i] != matrix.matrix[i])
                return true;
        return false;
    }
    // bool operator>(Matrix &matrix) {}
};
