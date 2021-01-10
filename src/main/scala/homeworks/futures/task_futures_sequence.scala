package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но, в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Тип возвращаемого значения функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правом - результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val resultsF =
      futures.foldLeft(Future.successful(List[A]())) {
        case (resultsF, fut) => fut.flatMap(x => resultsF.map(x :: _)).recoverWith(_ => resultsF)
      }
    val errorsF =
      futures.foldLeft(Future.successful(List[Throwable]())) {
        case (errorsF, fut) => fut.flatMap(_ => errorsF).recoverWith(e => errorsF.map(e :: _))
      }

    for {
      results <- resultsF
      errors <- errorsF
    } yield (results.reverse, errors.reverse)
  }
}
