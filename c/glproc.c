#include <string.h>
#include "glproc.h"

void* nvgLoadProc(const char* name, void* proc) {
    if (strcmp(name, "glGetError") == 0)
        return glGetError = proc;
    if (strcmp(name, "glGetUniformLocation") == 0)
        return glGetUniformLocation = proc;
    if (strcmp(name, "glCreateProgram") == 0)
        return glCreateProgram = proc;
    if (strcmp(name, "glCreateShader") == 0)
        return glCreateShader = proc;
    if (strcmp(name, "glGetUniformBlockIndex") == 0)
        return glGetUniformBlockIndex = proc;
    if (strcmp(name, "glActiveTexture") == 0)
        return glActiveTexture = proc;
    if (strcmp(name, "glAttachShader") == 0)
        return glAttachShader = proc;
    if (strcmp(name, "glBindAttribLocation") == 0)
        return glBindAttribLocation = proc;
    if (strcmp(name, "glBindBuffer") == 0)
        return glBindBuffer = proc;
    if (strcmp(name, "glBindBufferRange") == 0)
        return glBindBufferRange = proc;
    if (strcmp(name, "glBindTexture") == 0)
        return glBindTexture = proc;
    if (strcmp(name, "glBindVertexArray") == 0)
        return glBindVertexArray = proc;
    if (strcmp(name, "glBlendFuncSeparate") == 0)
        return glBlendFuncSeparate = proc;
    if (strcmp(name, "glBufferData") == 0)
        return glBufferData = proc;
    if (strcmp(name, "glColorMask") == 0)
        return glColorMask = proc;
    if (strcmp(name, "glCompileShader") == 0)
        return glCompileShader = proc;
    if (strcmp(name, "glCullFace") == 0)
        return glCullFace = proc;
    if (strcmp(name, "glDeleteBuffers") == 0)
        return glDeleteBuffers = proc;
    if (strcmp(name, "glDeleteProgram") == 0)
        return glDeleteProgram = proc;
    if (strcmp(name, "glDeleteShader") == 0)
        return glDeleteShader = proc;
    if (strcmp(name, "glDeleteTextures") == 0)
        return glDeleteTextures = proc;
    if (strcmp(name, "glDeleteVertexArrays") == 0)
        return glDeleteVertexArrays = proc;
    if (strcmp(name, "glDisable") == 0)
        return glDisable = proc;
    if (strcmp(name, "glDisableVertexAttribArray") == 0)
        return glDisableVertexAttribArray = proc;
    if (strcmp(name, "glDrawArrays") == 0)
        return glDrawArrays = proc;
    if (strcmp(name, "glEnable") == 0)
        return glEnable = proc;
    if (strcmp(name, "glEnableVertexAttribArray") == 0)
        return glEnableVertexAttribArray = proc;
    if (strcmp(name, "glFinish") == 0)
        return glFinish = proc;
    if (strcmp(name, "glFrontFace") == 0)
        return glFrontFace = proc;
    if (strcmp(name, "glGenBuffers") == 0)
        return glGenBuffers = proc;
    if (strcmp(name, "glGenTextures") == 0)
        return glGenTextures = proc;
    if (strcmp(name, "glGenVertexArrays") == 0)
        return glGenVertexArrays = proc;
    if (strcmp(name, "glGenerateMipmap") == 0)
        return glGenerateMipmap = proc;
    if (strcmp(name, "glGetIntegerv") == 0)
        return glGetIntegerv = proc;
    if (strcmp(name, "glGetProgramInfoLog") == 0)
        return glGetProgramInfoLog = proc;
    if (strcmp(name, "glGetProgramiv") == 0)
        return glGetProgramiv = proc;
    if (strcmp(name, "glGetShaderInfoLog") == 0)
        return glGetShaderInfoLog = proc;
    if (strcmp(name, "glGetShaderiv") == 0)
        return glGetShaderiv = proc;
    if (strcmp(name, "glLinkProgram") == 0)
        return glLinkProgram = proc;
    if (strcmp(name, "glPixelStorei") == 0)
        return glPixelStorei = proc;
    if (strcmp(name, "glShaderSource") == 0)
        return glShaderSource = proc;
    if (strcmp(name, "glStencilFunc") == 0)
        return glStencilFunc = proc;
    if (strcmp(name, "glStencilMask") == 0)
        return glStencilMask = proc;
    if (strcmp(name, "glStencilOp") == 0)
        return glStencilOp = proc;
    if (strcmp(name, "glStencilOpSeparate") == 0)
        return glStencilOpSeparate = proc;
    if (strcmp(name, "glTexImage2D") == 0)
        return glTexImage2D = proc;
    if (strcmp(name, "glTexParameteri") == 0)
        return glTexParameteri = proc;
    if (strcmp(name, "glTexSubImage2D") == 0)
        return glTexSubImage2D = proc;
    if (strcmp(name, "glUniform1f") == 0)
        return glUniform1f = proc;
    if (strcmp(name, "glUniform1fv") == 0)
        return glUniform1fv = proc;
    if (strcmp(name, "glUniform1i") == 0)
        return glUniform1i = proc;
    if (strcmp(name, "glUniform1iv") == 0)
        return glUniform1iv = proc;
    if (strcmp(name, "glUniform2f") == 0)
        return glUniform2f = proc;
    if (strcmp(name, "glUniform2fv") == 0)
        return glUniform2fv = proc;
    if (strcmp(name, "glUniform2i") == 0)
        return glUniform2i = proc;
    if (strcmp(name, "glUniform2iv") == 0)
        return glUniform2iv = proc;
    if (strcmp(name, "glUniform3f") == 0)
        return glUniform3f = proc;
    if (strcmp(name, "glUniform3fv") == 0)
        return glUniform3fv = proc;
    if (strcmp(name, "glUniform3i") == 0)
        return glUniform3i = proc;
    if (strcmp(name, "glUniform3iv") == 0)
        return glUniform3iv = proc;
    if (strcmp(name, "glUniform4f") == 0)
        return glUniform4f = proc;
    if (strcmp(name, "glUniform4fv") == 0)
        return glUniform4fv = proc;
    if (strcmp(name, "glUniform4i") == 0)
        return glUniform4i = proc;
    if (strcmp(name, "glUniform4iv") == 0)
        return glUniform4iv = proc;
    if (strcmp(name, "glUniformBlockBinding") == 0)
        return glUniformBlockBinding = proc;
    if (strcmp(name, "glUniformMatrix2fv") == 0)
        return glUniformMatrix2fv = proc;
    if (strcmp(name, "glUniformMatrix3fv") == 0)
        return glUniformMatrix3fv = proc;
    if (strcmp(name, "glUniformMatrix4fv") == 0)
        return glUniformMatrix4fv = proc;
    if (strcmp(name, "glUseProgram") == 0)
        return glUseProgram = proc;
    if (strcmp(name, "glVertexAttribPointer") == 0)
        return glVertexAttribPointer = proc;
    if (strcmp(name, "glBindFramebuffer") == 0)
        return glBindFramebuffer = proc;
    if (strcmp(name, "glBindRenderbuffer") == 0)
        return glBindRenderbuffer = proc;
    if (strcmp(name, "glCheckFramebufferStatus") == 0)
        return glCheckFramebufferStatus = proc;
    if (strcmp(name, "glDeleteFramebuffers") == 0)
        return glDeleteFramebuffers = proc;
    if (strcmp(name, "glDeleteRenderbuffers") == 0)
        return glDeleteRenderbuffers = proc;
    if (strcmp(name, "glFramebufferRenderbuffer") == 0)
        return glFramebufferRenderbuffer = proc;
    if (strcmp(name, "glFramebufferTexture2D") == 0)
        return glFramebufferTexture2D = proc;
    if (strcmp(name, "glGenFramebuffers") == 0)
        return glGenFramebuffers = proc;
    if (strcmp(name, "glGenRenderbuffers") == 0)
        return glGenRenderbuffers = proc;
    if (strcmp(name, "glRenderbufferStorage") == 0)
        return glRenderbufferStorage = proc;
    return NULL;
}
