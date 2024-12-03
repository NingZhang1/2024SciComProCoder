#ifndef MATRIX_HPP
#define MATRIX_HPP
#include <vector>
#include <math.h>
#include <iomanip>

//////////////////////////////////////////////////////////////////
//          Simple classes for Matrix and Vector                //
// It is just a substitution of Eigen with the simplest weight. //
//                    Writen by Yang Guo                        //
//////////////////////////////////////////////////////////////////

template <class T> class Vector;

template <class T>
class Matrix{
    public:

        int _rows;
        int _cols;
        std::vector<std::vector<T> > array;
        Matrix<T>(int rows, int cols);
        Matrix<T>();
        ~Matrix<T>();

        void copy(const Matrix<T>& matrix);
        void resize(int newrows, int newcoles);
        void fill(const T& value);
        T& operator()(int x, int y);
        void print() const;

        int rows() const;
        int cols() const;
        int size() const;
        Vector<T> row(int x) const;
        Vector<T> col(int x) const;

        Matrix<T> add(const Matrix<T>& m) const;
        Matrix<T> subtract(const Matrix<T>& m) const;
        Matrix<T> divide(const T& value) const;
        Matrix<T> multiply(const T& value) const;

        Matrix<T> multiply(const Matrix& m) const;
        Matrix<T> transpose() const;
        static const Matrix<T> Zero(int newrows, int newcoles);

        Matrix<T> operator+=(const Matrix<T>& m);
        Matrix<T> operator-=(const Matrix<T>& m);
        Matrix<T> operator/=(const T &m);
        Matrix<T> operator*=(const T &m);

        Matrix<T> operator*=(const Matrix<T>& m);
};

template <class T> Matrix<T> operator+(const Matrix<T>& a, const Matrix<T>& b);
template <class T> Matrix<T> operator-(const Matrix<T>& a, const Matrix<T>& b);
template <class T> Matrix<T> operator/(const Matrix<T>& a, const T &b);
template <class T> Matrix<T> operator*(const T &b, const Matrix<T>& a);
template <class T> Matrix<T> operator*(const Matrix<T>& a, const T &b);
template <class T> Matrix<T> operator*(const Matrix<T>& a, const Matrix<T>& b);

template <class T>
Matrix<T>::Matrix(int rows, int cols){
    this->_rows = rows;
    this->_cols = cols;
    this->array = std::vector<std::vector<T> >(rows, std::vector<T>(cols));
    if(rows>0 && cols>0) {
      this->array = std::vector<std::vector<T> >(_rows, std::vector<T>(_cols));
      this->fill(0.0);
    }
}

template <class T>
Matrix<T>::Matrix(){
    _rows = 0;
    _cols = 0;
}

template <class T>
Matrix<T>::~Matrix(){
    this->resize(0,0);
    _rows = 0;
    _cols = 0;
}

template <class T>
void Matrix<T>::copy(const Matrix<T>& matrix){
    if(!(matrix.rows()>0 && matrix.cols()>0))
        throw std::invalid_argument("Size of array must be greater than 0.");

    this->_rows = matrix.rows();
    this->_cols = matrix.cols();
    this->array = std::vector<std::vector<T> >(_rows, std::vector<T>(_cols));

    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            array[i][j] = matrix.array[i][j];
        }
    }
}

template <class T>
int Matrix<T>::rows() const{
    return _rows;
}

template <class T>
int Matrix<T>::cols() const{
    return _cols;
}

template <class T>
int Matrix<T>::size() const{
    return _rows*_cols;
}

template <class T>
Vector<T> Matrix<T>::row(int x) const{
    Vector<T> result(_cols);

    for (int j=0 ; j<_cols ; j++){
        result(j) = array[x][j];
    }
    return result;
}

template <class T>
Vector<T> Matrix<T>::col(int x) const{
    Vector<T> result(_rows);
 
    for (int i=0 ; i<_rows ; i++){
        result(i) = array[i][x];
    }
    return result;
}

template <class T>
T& Matrix<T>::operator()(int x, int y){
    if(!(x>=0 && x<_rows && y>=0 && y<_cols))
        throw std::invalid_argument("Index out of bounds.");
    return array[x][y];
}

template <class T>
void Matrix<T>::fill(const T& value){
    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            array[i][j] = value;
        }
    }
}

template <class T>
void Matrix<T>::resize(int newrows, int newcols) {
    if(!(newrows>=0 && newcols>=0))
        throw std::invalid_argument("Index out of bounds in resize");
    this->_rows = newrows;
    this->_cols = newcols;
    if(newrows>0 && newcols>0) {
      this->array = std::vector<std::vector<T> >(_rows, std::vector<T>(_cols));
      this->fill(0.0);
    }
}

template <class T>
Matrix<T> Matrix<T>::add(const Matrix& m) const{
    if(!(_rows==m._rows && _cols==m._cols))
        throw std::invalid_argument("Matrix dimension must be the same.");

    Matrix result(_rows, _cols);
    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            result.array[i][j] = array[i][j] + m.array[i][j];
        }
    }

    return result;
}

template <class T>
Matrix<T> Matrix<T>::subtract(const Matrix& m) const{
    if(!(_rows==m._rows && _cols==m._cols))
        throw std::invalid_argument("Matrix dimension must be the same.");

    Matrix result(_rows, _cols);
    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            result.array[i][j] = array[i][j] - m.array[i][j];
        }
    }
    return result;
}

template <class T>
Matrix<T> Matrix<T>::divide(const T& value) const{
    Matrix result;
    result.copy(*this);
    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            result.array[i][j] /= value;
        }
    }

    return result;
}

template <class T>
Matrix<T> Matrix<T>::multiply(const T& value) const{
    Matrix result;
    result.copy(*this);
    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            result.array[i][j] *= value;
        }
    }

    return result;
}

template <class T>
Matrix<T> Matrix<T>::multiply(const Matrix& m) const{
    if(!(_cols==m._rows))
        throw std::invalid_argument("Matrix dimension must be the same.");

    Matrix result(_rows, m._cols);

    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<m._cols ; j++){
            for (int k=0 ; k<_cols ; k++){
               result.array[i][j] = array[i][k] * m.array[k][j];
            }
        }
    }
    return result;
}

template <class T>
Matrix<T> operator*(const Matrix<T>& a, const Matrix<T>& b){
    return a.multiply(b);
}

template <class T>
Matrix<T> operator+(const Matrix<T>& a, const Matrix<T>& b){
    return a.add(b);
}

template <class T>
Matrix<T> operator-(const Matrix<T>& a, const Matrix<T>& b){
    return a.subtract(b);
}

template <class T>
Matrix<T> operator/(const Matrix<T>& a, const T &b){
    return a.divide(b);
}

template <class T>
Matrix<T> operator*(const T &b, const Matrix<T>& a){
    return a.multiply(b);
}

template <class T>
Matrix<T> operator*(const Matrix<T>& a, const T &b){
    return a.multiply(b);
}

template <class T>
Matrix<T> Matrix<T>::operator+=(const Matrix& m){
    this->array = add(m).array;
    return *this;
}

template <class T>
Matrix<T> Matrix<T>::operator-=(const Matrix& m){
    this->array = subtract(m).array;
    return *this;
}

template <class T>
Matrix<T> Matrix<T>::operator/=(const T &m){
    *this = this->divide(m);
    return *this;
}


template <class T>
Matrix<T> Matrix<T>::operator*=(const T &m){
    *this = this->multiply(m);
    return *this;
}

template <class T>
Matrix<T> Matrix<T>::operator*=(const Matrix& m){
    this->array = multiply(m).array;
    this->_cols = m._cols;
    return *this;
}

template <class T>
Matrix<T> Matrix<T>::transpose() const{
    Matrix<T> result(_cols, _rows);

    for (int i=0 ; i<_rows ; i++){
        for (int j=0 ; j<_cols ; j++){
            result.array[j][i] = array[i][j];
        }
    }
    return result;
}

template <class T>
const Matrix<T> Matrix<T>::Zero(int newrows, int newcols) {
    if(!(newrows>=0 && newcols>=0))
        throw std::invalid_argument("Index out of bounds in Zero");
    Matrix<T> result(newrows, newcols);
    if(newrows>0 && newcols>0) {
      result.fill(0.0);
    }
    return result;
}

template <class T>
void Matrix<T>::print() const{
    std::cout << "Rows: " << this->_rows << std::endl;
    std::cout << "Cols: " << this->_cols << std::endl;

    std::cout << std::scientific;

    for (int i = 0; i < this->_rows; i++) {
      for (int j = 0; j < this->_cols; j++) {
          std::cout <<  this->array[i][j] << "  ";
      }
      std::cout << std::endl;
    }
}

typedef Matrix<double>  MatrixXd;
typedef Matrix<int>     MatrixXi;

//////////////////////////////////////////////////////////////////
//          Simple classes for Matrix and Vector                //
// It is just a substitution of Eigen with the simplest weight. //
//                    Writen by Yang Guo                        //
//////////////////////////////////////////////////////////////////

template <class T>
class Vector{

    public:
        int _rows;
        std::vector<T> array;
        Vector<T>(int rows);
        Vector<T>();
        ~Vector<T>();

        void copy(const Matrix<T>& matrix);
        void copy(const Vector<T>& vector);
        void resize(int newrows);
        void fill(const T& value);
        T& operator()(int x);
        void print() const;
        static const Vector<T> Zero(int newrows); 

        int  rows() const;
        int  size() const;

        Vector<T> add(const Vector<T>& m) const;
        Vector<T> subtract(const Vector<T>& m) const;
        Vector<T> multiply(const T& value) const;
        Vector<T> divide(const T& value) const;

        Vector<T> operator+=(const Vector<T>& m);
        Vector<T> operator-=(const Vector<T>& m);
        Vector<T> operator*=(const T &m);
        Vector<T> operator/=(const T &m);

        double norm() const;
        double dot(Vector<T> vec) const;

};

template <class T> Vector<T> operator+(const Vector<T>& a, const Vector<T>& b);
template <class T> Vector<T> operator-(const Vector<T>& a, const Vector<T>& b);
template <class T> Vector<T> operator*(const T &b, const Vector<T>& a);
template <class T> Vector<T> operator*(const Vector<T>& a, const T &b);
template <class T> Vector<T> operator/(const Vector<T>& a, const T &b);

template <class T>
Vector<T>::Vector(int rows){
    this->_rows  = rows;
    this->array = std::vector<T>(_rows);
    if(_rows>0) {
      this->array = std::vector<T> (_rows);
      this->fill(0.0);
    }
}

template <class T>
Vector<T>::Vector(){
    _rows = 0;
}

template <class T>
Vector<T>::~Vector(){
    this->resize(0);
    _rows = 0;
}

template <class T>
int Vector<T>::rows() const{
    return _rows;
}

template <class T>
int Vector<T>::size() const{
    return _rows;
}

template <class T>
Vector<T> Vector<T>::add(const Vector& m) const{
    if(!(_rows==m._rows))
        throw std::invalid_argument("Vector dimension must be the same.");

    Vector result(_rows);
    for (int i=0 ; i<_rows ; i++){
        result.array[i] = array[i] + m.array[i];
    }

    return result;
}

template <class T>
Vector<T> Vector<T>::subtract(const Vector& m) const{
    if(!(_rows==m._rows))
        throw std::invalid_argument("Vector dimension must be the same.");

    Vector result(_rows);
    for (int i=0 ; i<_rows ; i++){
        result.array[i] = array[i] - m.array[i];
    }
    return result;
}

template <class T>
Vector<T> Vector<T>::multiply(const T& value) const{
    Vector result;
    result.copy(*this);
    for (int i=0 ; i<_rows ; i++){
        result.array[i] *= value;
    }

    return result;
}

template <class T>
Vector<T> Vector<T>::divide(const T& value) const{
    Vector result;
    result.copy(*this);
    for (int i=0 ; i<_rows ; i++){
        result.array[i] /= value;
    }

    return result;
}

template <class T>
Vector<T> operator+(const Vector<T>& a, const Vector<T>& b){
    return a.add(b);
}

template <class T>
Vector<T> operator-(const Vector<T>& a, const Vector<T>& b){
    return a.subtract(b);
}

template <class T>
Vector<T> operator*(const T &b, const Vector<T>& a){
    return a.multiply(b);
}

template <class T>
Vector<T> operator*(const Vector<T>& a, const T &b){
    return a.multiply(b);
}

template <class T>
Vector<T> operator/(const Vector<T>& a, const T &b){
    return a.divide(b);
}

template <class T>
Vector<T> Vector<T>::operator+=(const Vector& m){
    this->array = add(m).array;
    return *this;
}

template <class T>
Vector<T> Vector<T>::operator-=(const Vector& m){
    this->array = subtract(m).array;
    return *this;
}

template <class T>
Vector<T> Vector<T>::operator*=(const T &m){
    *this = this->multiply(m);
    return *this;
}

template <class T>
Vector<T> Vector<T>::operator/=(const T &m){
    *this = this->divide(m);
    return *this;
}

template <class T>
double Vector<T>::norm() const{
    double n=0.0;
    for (int i=0 ; i<_rows ; i++){
       n+=array[i]*array[i];
    }
    n=sqrt(n);
    return n;
}

template <class T>
double Vector<T>::dot(Vector<T> vec) const{
    if(!( vec._rows == this->_rows ))
        throw std::invalid_argument("Indices of two vector do not match");
    double n=0.0;
    for (int i=0 ; i<_rows ; i++){
       n+=this->array[i]*vec(i);
    }
    return n;
}

template <class T>
T& Vector<T>::operator()(int x){
    if(!( x>=0 && x<_rows ))
        throw std::invalid_argument("Index out of bounds.");
    return array[x];
}

template <class T>
void Vector<T>::resize(int newrows) {
    if(!(newrows>=0))
        throw std::invalid_argument("Index out of bounds in resize");

    this->_rows = newrows;
    if(newrows>0) {
      this->array = std::vector<T>(newrows);
      this->fill(0.0);
    }

}

template <class T>
void Vector<T>::fill(const T& value){
    for (int i=0 ; i<_rows ; i++){
        array[i] = value;
    }
}

template <class T>
void Vector<T>::print() const{
    std::cout << "Rows: " << this->_rows << std::endl;
    std::cout << std::scientific;

    for (int i = 0; i < this->_rows; i++) {
        std::cout << this->array[i] << "  ";
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

template <class T>
void Vector<T>::copy(const Vector<T>& vector){
    if(!(vector.rows()>0))
        throw std::invalid_argument("Size of array must be greater than 0.");

    this->_rows = vector.rows();
    this->array = std::vector<T> (_rows);

    for (int i=0 ; i<_rows ; i++){
         array[i] = vector.array[i];
    }
}

template <class T>
void Vector<T>::copy(const Matrix<T>& matrix){
    if(!(matrix.rows()>0 && matrix.cols()>0))
        throw std::invalid_argument("Size of array must be greater than 0.");

    this->_rows = matrix.rows()*matrix.cols();
    this->array = std::vector<T> (_rows);

    for (int i=0 ; i<matrix.rows() ; i++){
        for (int j=0 ; j<matrix.cols() ; j++){
            array[i*matrix.rows()+j] = matrix.array[i][j];
        }
    }
}

template <class T>
const Vector<T> Vector<T>::Zero(int newrows) {
    if(!(newrows>=0))
        throw std::invalid_argument("Index out of bounds in Zero");
    Vector<T> result(newrows);
    if(newrows>0) {
      result.fill(0.0);
    }
    return result;
}

typedef Vector<double>  VectorXd;
typedef Vector<int>     VectorXi;

#endif
