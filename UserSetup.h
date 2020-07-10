/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  06/28/16             */
/*                                                     */
/*               USER SETUP HEADER FILE                */
/*******************************************************/
#ifndef USER_SETUP_H__
#define USER_SETUP_H__

template<typename T>
inline void zeroMemory(T* container) noexcept {
    char* tmpBuf = (char*)container;
    for (int i = 0; i < sizeof(T); ++i) {
        tmpBuf[i] = 0;
    }
}
#endif