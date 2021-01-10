package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */

  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
    assert(word.forall((c: Char) => 'A' <= c && c <= 'Z'))

    val normalizedOffset = offset % 26
    word.map((c: Char) => ((c - 'A' + normalizedOffset) % 26 + 'A').toChar)
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    assert(cipher.forall((c: Char) => 'A' <= c && c <= 'Z'))

    val normalizedOffset = 26 - offset % 26
    cipher.map((c: Char) => ((c - 'A' + normalizedOffset) % 26 + 'A').toChar)
  }
}
