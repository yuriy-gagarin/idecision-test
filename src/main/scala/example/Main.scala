package example

import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.collection.AbstractIterator

/*
1) implement a retry method that accepts a function and a number of attempts; 
   should retry up to that number of attempts if function fails, then throw exception
*/

object RetryFunction {

  // Retry function
  //

  // Function accepts a `by name` value that has a possibility of throwing.
  // It will reattempt evaluation of the value up to `maxAttempts` times in case of an exception.

  def retry[A](f: => A, maxAttempts: Int): A = {
    @tailrec def retryIter(attempts: Int): A = {
      Try(f) match {
        case Success(value) => value
        case Failure(_) if attempts < maxAttempts => retryIter(attempts+1)
        case Failure(exception) => throw exception
      }
    }

    retryIter(0)
  }
}

/*
2) implement a class extending Iterator which wraps another iterator (potentially over a huge amount of data)
   and also adds a peek() method which shows the next element but doesn’t advance the pointer
*/

object ExtendedIterator {

  // Iterator with a peek method
  // 

  // NOTE: A similar class already exists in the standart library: `scala.collection.BufferedIterator`

  trait PeekingIterator[A] extends Iterator[A] {
    def peek: Option[A]
  }

  def peeking[A](it: Iterator[A]): PeekingIterator[A] = new PeekingIterator[A] {
    private var peeked: Option[A] = None

    def hasNext: Boolean = peeked.isDefined || it.hasNext

    def next(): A = peeked match {
      case Some(value) => {
        peeked = None
        value
      }
      case None => it.next()
    }

    def peek: Option[A] = peeked match {
      case _ if !hasNext => None
      case p@Some(v) => p
      case None => {
        peeked = Some(it.next())
        peeked
      }
    }
  }
}




