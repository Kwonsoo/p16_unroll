; ModuleID = 'spell-1.0.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i32, [44 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
%struct.str = type { i8*, i32, i32 }
%struct.stat = type { i64, i16, i64, i32, i32, i32, i32, i64, i16, i64, i64, i64, %struct.timespec, %struct.timespec, %struct.timespec, i64, i64 }
%struct.timespec = type { i64, i64 }
%struct.pipe = type { i32, i32, i32, i32, i32, i32, %struct.__anonstruct_fd_set_13 }
%struct.__anonstruct_fd_set_13 = type { [16 x i64] }
%struct.option = type { i8*, i32, i32*, i32 }
%struct.__va_list_tag = type { i32, i32, i8*, i8* }
%struct.timeval = type { i64, i64 }

@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@version = constant [12 x i8] c"version 1.0\00", align 1
@.str1 = private unnamed_addr constant [11 x i8] c"all-chains\00", align 1
@.str2 = private unnamed_addr constant [8 x i8] c"british\00", align 1
@.str3 = private unnamed_addr constant [11 x i8] c"dictionary\00", align 1
@.str4 = private unnamed_addr constant [5 x i8] c"help\00", align 1
@.str5 = private unnamed_addr constant [7 x i8] c"ispell\00", align 1
@.str6 = private unnamed_addr constant [15 x i8] c"ispell-version\00", align 1
@.str7 = private unnamed_addr constant [7 x i8] c"number\00", align 1
@.str8 = private unnamed_addr constant [16 x i8] c"print-file-name\00", align 1
@.str9 = private unnamed_addr constant [12 x i8] c"print-stems\00", align 1
@.str10 = private unnamed_addr constant [10 x i8] c"stop-list\00", align 1
@.str11 = private unnamed_addr constant [8 x i8] c"verbose\00", align 1
@.str12 = private unnamed_addr constant [8 x i8] c"version\00", align 1
@long_options = constant <{ { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] } }> <{ { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([11 x i8]* @.str1, i32 0, i32 0), i32 0, i32* null, i32 108, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([8 x i8]* @.str2, i32 0, i32 0), i32 0, i32* null, i32 98, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([11 x i8]* @.str3, i32 0, i32 0), i32 1, i32* null, i32 100, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([5 x i8]* @.str4, i32 0, i32 0), i32 0, i32* null, i32 104, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([7 x i8]* @.str5, i32 0, i32 0), i32 1, i32* null, i32 105, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([15 x i8]* @.str6, i32 0, i32 0), i32 0, i32* null, i32 73, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([7 x i8]* @.str7, i32 0, i32 0), i32 0, i32* null, i32 110, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([16 x i8]* @.str8, i32 0, i32 0), i32 0, i32* null, i32 111, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([12 x i8]* @.str9, i32 0, i32 0), i32 0, i32* null, i32 120, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([10 x i8]* @.str10, i32 0, i32 0), i32 1, i32* null, i32 115, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([8 x i8]* @.str11, i32 0, i32 0), i32 0, i32* null, i32 118, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* getelementptr inbounds ([8 x i8]* @.str12, i32 0, i32 0), i32 0, i32* null, i32 86, [4 x i8] undef }, { i8*, i32, i32*, i32, [4 x i8] } { i8* null, i32 0, i32* null, i32 0, [4 x i8] undef } }>, align 16
@program_name = global i8* null, align 8
@ispell_prog = global i8* null, align 8
@dictionary = global i8* null, align 8
@show_ispell_version = global i32 0, align 4
@read_stdin = global i32 0, align 4
@british = global i32 0, align 4
@verbose = global i32 0, align 4
@number_lines = global i32 0, align 4
@print_file_names = global i32 0, align 4
@interactive = global i32 0, align 4
@.str13 = private unnamed_addr constant [13 x i8] c"IVbdhilnosvx\00", align 1
@optarg = external global i8*
@.str14 = private unnamed_addr constant [26 x i8] c"option argument not given\00", align 1
@.str15 = private unnamed_addr constant [39 x i8] c"Try `%s --help' for more information.\0A\00", align 1
@.str16 = private unnamed_addr constant [33 x i8] c"Usage: %s [OPTION]... [FILE]...\0A\00", align 1
@.str17 = private unnamed_addr constant [687 x i8] c"This is GNU Spell, a Unix spell emulator.\0A\0A  -I, --ispell-version\09\09Print Ispell's version.\0A  -V, --version\09\09\09Print the version number.\0A  -b, --british\09\09\09Use the British dictionary.\0A  -d, --dictionary=FILE\09\09Use FILE to look up words.\0A  -h, --help\09\09\09Print a summary of the options.\0A  -i, --ispell=PROGRAM\09\09Calls PROGRAM as Ispell.\0A  -l, --all-chains\09\09Ignored; for compatibility.\0A  -n, --number\09\09\09Print line numbers before lines.\0A  -o, --print-file-name\09\09Print file names before lines.\0A  -s, --stop-list=FILE\09\09Ignored; for compatibility.\0A  -v, --verbose\09\09\09Print words not literally found.\0A  -x, --print-stems\09\09Ignored; for compatibility.\0A\0APlease use Info to read more (type `info spell').\0A\00", align 1
@stderr = external global %struct._IO_FILE*
@.str18 = private unnamed_addr constant [28 x i8] c"error forking to run Ispell\00", align 1
@.str19 = private unnamed_addr constant [5 x i8] c"PATH\00", align 1
@.str20 = private unnamed_addr constant [24 x i8] c"unable to locate Ispell\00", align 1
@.str21 = private unnamed_addr constant [23 x i8] c"%s: error reading line\00", align 1
@.str22 = private unnamed_addr constant [24 x i8] c"error writing to Ispell\00", align 1
@.str23 = private unnamed_addr constant [16 x i8] c"%s: close error\00", align 1
@.str24 = private unnamed_addr constant [4 x i8] c"%s:\00", align 1
@.str25 = private unnamed_addr constant [5 x i8] c"%d: \00", align 1
@.str26 = private unnamed_addr constant [30 x i8] c"unrecognized Ispell line `%s'\00", align 1
@.str27 = private unnamed_addr constant [35 x i8] c"premature EOF from Ispell's stderr\00", align 1
@.str28 = private unnamed_addr constant [12 x i8] c"Can't open \00", align 1
@.str29 = private unnamed_addr constant [16 x i8] c"%s: cannot open\00", align 1
@.str30 = private unnamed_addr constant [8 x i8] c"%s: %s\0A\00", align 1
@.str31 = private unnamed_addr constant [31 x i8] c"error creating SIGPIPE handler\00", align 1
@.str32 = private unnamed_addr constant [31 x i8] c"error creating SIGCHLD handler\00", align 1
@.str33 = private unnamed_addr constant [30 x i8] c"error creating pipe to Ispell\00", align 1
@optind = external global i32
@.str34 = private unnamed_addr constant [35 x i8] c"premature EOF from Ispell's stdout\00", align 1
@.str35 = private unnamed_addr constant [23 x i8] c"%s: Ispell version %s\0A\00", align 1
@.str36 = private unnamed_addr constant [2 x i8] c"-\00", align 1
@stdin = external global %struct._IO_FILE*
@.str37 = private unnamed_addr constant [15 x i8] c"%s: stat error\00", align 1
@.str38 = private unnamed_addr constant [19 x i8] c"%s: is a directory\00", align 1
@.str39 = private unnamed_addr constant [2 x i8] c"r\00", align 1
@.str40 = private unnamed_addr constant [15 x i8] c"%s: open error\00", align 1
@.str41 = private unnamed_addr constant [22 x i8] c"error duping to stdin\00", align 1
@.str42 = private unnamed_addr constant [23 x i8] c"error duping to stdout\00", align 1
@.str43 = private unnamed_addr constant [23 x i8] c"error duping to stderr\00", align 1
@.str44 = private unnamed_addr constant [3 x i8] c"-a\00", align 1
@.str45 = private unnamed_addr constant [3 x i8] c"-p\00", align 1
@.str46 = private unnamed_addr constant [19 x i8] c"error executing %s\00", align 1
@.str47 = private unnamed_addr constant [3 x i8] c"-d\00", align 1
@.str48 = private unnamed_addr constant [25 x i8] c"virtual memory exhausted\00", align 1
@stdout = external global %struct._IO_FILE*
@.str49 = private unnamed_addr constant [5 x i8] c"%s: \00", align 1
@.str50 = private unnamed_addr constant [5 x i8] c": %s\00", align 1
@.str51 = private unnamed_addr constant [12 x i8] c"Ispell died\00", align 1
@.str52 = private unnamed_addr constant [12 x i8] c"broken pipe\00", align 1

; Function Attrs: nounwind uwtable
define %struct.str* @str_make(%struct.str* %str) #0 {
  %1 = alloca %struct.str*, align 8
  %tmp = alloca i8*, align 8
  %tmp___0 = alloca i32, align 4
  %tmp___1 = alloca i8*, align 8
  %tmp___2 = alloca i32, align 4
  %tmp___3 = alloca i8*, align 8
  store %struct.str* %str, %struct.str** %1, align 8
  %2 = load %struct.str** %1, align 8
  %3 = icmp ne %struct.str* %2, null
  br i1 %3, label %16, label %4

; <label>:4                                       ; preds = %0
  %5 = call i8* @xmalloc(i32 16)
  store i8* %5, i8** %tmp, align 8
  %6 = load i8** %tmp, align 8
  %7 = bitcast i8* %6 to %struct.str*
  store %struct.str* %7, %struct.str** %1, align 8
  store i32 64, i32* %tmp___0, align 4
  %8 = load i32* %tmp___0, align 4
  %9 = load %struct.str** %1, align 8
  %10 = getelementptr inbounds %struct.str* %9, i32 0, i32 2
  store i32 %8, i32* %10, align 4
  %11 = load i32* %tmp___0, align 4
  %12 = call i8* @xmalloc(i32 %11)
  store i8* %12, i8** %tmp___1, align 8
  %13 = load i8** %tmp___1, align 8
  %14 = load %struct.str** %1, align 8
  %15 = getelementptr inbounds %struct.str* %14, i32 0, i32 0
  store i8* %13, i8** %15, align 8
  br label %28

; <label>:16                                      ; preds = %0
  store i32 64, i32* %tmp___2, align 4
  %17 = load i32* %tmp___2, align 4
  %18 = load %struct.str** %1, align 8
  %19 = getelementptr inbounds %struct.str* %18, i32 0, i32 2
  store i32 %17, i32* %19, align 4
  %20 = load %struct.str** %1, align 8
  %21 = getelementptr inbounds %struct.str* %20, i32 0, i32 0
  %22 = load i8** %21, align 8
  %23 = load i32* %tmp___2, align 4
  %24 = call i8* @xrealloc(i8* %22, i32 %23)
  store i8* %24, i8** %tmp___3, align 8
  %25 = load i8** %tmp___3, align 8
  %26 = load %struct.str** %1, align 8
  %27 = getelementptr inbounds %struct.str* %26, i32 0, i32 0
  store i8* %25, i8** %27, align 8
  br label %28

; <label>:28                                      ; preds = %16, %4
  %29 = load %struct.str** %1, align 8
  %30 = getelementptr inbounds %struct.str* %29, i32 0, i32 1
  store i32 0, i32* %30, align 4
  %31 = load %struct.str** %1, align 8
  ret %struct.str* %31
}

; Function Attrs: nounwind uwtable
define internal i8* @xmalloc(i32 %size) #0 {
  %1 = alloca i32, align 4
  %ptr = alloca i8*, align 8
  %tmp = alloca i8*, align 8
  store i32 %size, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = call noalias i8* @malloc(i32 %2)
  store i8* %3, i8** %tmp, align 8
  %4 = load i8** %tmp, align 8
  store i8* %4, i8** %ptr, align 8
  %5 = load i8** %ptr, align 8
  %6 = icmp ne i8* %5, null
  br i1 %6, label %8, label %7

; <label>:7                                       ; preds = %0
  call void (i32, i32, i8*, ...)* @error(i32 1, i32 0, i8* getelementptr inbounds ([25 x i8]* @.str48, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i8** %ptr, align 8
  ret i8* %9
}

; Function Attrs: nounwind uwtable
define internal i8* @xrealloc(i8* %ptr, i32 %size) #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  %tmp = alloca i8*, align 8
  store i8* %ptr, i8** %2, align 8
  store i32 %size, i32* %3, align 4
  %4 = load i8** %2, align 8
  %5 = icmp ne i8* %4, null
  br i1 %5, label %10, label %6

; <label>:6                                       ; preds = %0
  %7 = load i32* %3, align 4
  %8 = call i8* @xmalloc(i32 %7)
  store i8* %8, i8** %tmp, align 8
  %9 = load i8** %tmp, align 8
  store i8* %9, i8** %1
  br label %19

; <label>:10                                      ; preds = %0
  %11 = load i8** %2, align 8
  %12 = load i32* %3, align 4
  %13 = call noalias i8* @realloc(i8* %11, i32 %12)
  store i8* %13, i8** %2, align 8
  %14 = load i8** %2, align 8
  %15 = icmp ne i8* %14, null
  br i1 %15, label %17, label %16

; <label>:16                                      ; preds = %10
  call void (i32, i32, i8*, ...)* @error(i32 1, i32 0, i8* getelementptr inbounds ([25 x i8]* @.str48, i32 0, i32 0))
  br label %17

; <label>:17                                      ; preds = %16, %10
  %18 = load i8** %2, align 8
  store i8* %18, i8** %1
  br label %19

; <label>:19                                      ; preds = %17, %6
  %20 = load i8** %1
  ret i8* %20
}

; Function Attrs: nounwind uwtable
define void @str_add_char(%struct.str* %str, i8 signext %c) #0 {
  %1 = alloca %struct.str*, align 8
  %2 = alloca i8, align 1
  %tmp = alloca i32, align 4
  %tmp___0 = alloca i8*, align 8
  store %struct.str* %str, %struct.str** %1, align 8
  store i8 %c, i8* %2, align 1
  %3 = load %struct.str** %1, align 8
  %4 = icmp ne %struct.str* %3, null
  br i1 %4, label %8, label %5

; <label>:5                                       ; preds = %0
  %6 = load %struct.str** %1, align 8
  %7 = call %struct.str* @str_make(%struct.str* %6)
  store %struct.str* %7, %struct.str** %1, align 8
  br label %17

; <label>:8                                       ; preds = %0
  %9 = load %struct.str** %1, align 8
  %10 = getelementptr inbounds %struct.str* %9, i32 0, i32 0
  %11 = load i8** %10, align 8
  %12 = icmp ne i8* %11, null
  br i1 %12, label %16, label %13

; <label>:13                                      ; preds = %8
  %14 = load %struct.str** %1, align 8
  %15 = call %struct.str* @str_make(%struct.str* %14)
  store %struct.str* %15, %struct.str** %1, align 8
  br label %16

; <label>:16                                      ; preds = %13, %8
  br label %17

; <label>:17                                      ; preds = %16, %5
  %18 = load %struct.str** %1, align 8
  %19 = getelementptr inbounds %struct.str* %18, i32 0, i32 1
  %20 = load i32* %19, align 4
  %21 = add nsw i32 %20, 1
  store i32 %21, i32* %19, align 4
  %22 = load %struct.str** %1, align 8
  %23 = getelementptr inbounds %struct.str* %22, i32 0, i32 1
  %24 = load i32* %23, align 4
  %25 = load %struct.str** %1, align 8
  %26 = getelementptr inbounds %struct.str* %25, i32 0, i32 2
  %27 = load i32* %26, align 4
  %28 = icmp ugt i32 %24, %27
  br i1 %28, label %29, label %45

; <label>:29                                      ; preds = %17
  %30 = load %struct.str** %1, align 8
  %31 = getelementptr inbounds %struct.str* %30, i32 0, i32 2
  %32 = load i32* %31, align 4
  %33 = add i32 %32, 64
  store i32 %33, i32* %tmp, align 4
  %34 = load i32* %tmp, align 4
  %35 = load %struct.str** %1, align 8
  %36 = getelementptr inbounds %struct.str* %35, i32 0, i32 2
  store i32 %34, i32* %36, align 4
  %37 = load %struct.str** %1, align 8
  %38 = getelementptr inbounds %struct.str* %37, i32 0, i32 0
  %39 = load i8** %38, align 8
  %40 = load i32* %tmp, align 4
  %41 = call i8* @xrealloc(i8* %39, i32 %40)
  store i8* %41, i8** %tmp___0, align 8
  %42 = load i8** %tmp___0, align 8
  %43 = load %struct.str** %1, align 8
  %44 = getelementptr inbounds %struct.str* %43, i32 0, i32 0
  store i8* %42, i8** %44, align 8
  br label %45

; <label>:45                                      ; preds = %29, %17
  %46 = load i8* %2, align 1
  %47 = load %struct.str** %1, align 8
  %48 = getelementptr inbounds %struct.str* %47, i32 0, i32 0
  %49 = load i8** %48, align 8
  %50 = load %struct.str** %1, align 8
  %51 = getelementptr inbounds %struct.str* %50, i32 0, i32 1
  %52 = load i32* %51, align 4
  %53 = sub nsw i32 %52, 1
  %54 = sext i32 %53 to i64
  %55 = getelementptr inbounds i8* %49, i64 %54
  store i8 %46, i8* %55, align 1
  ret void
}

; Function Attrs: nounwind uwtable
define void @str_add_str(%struct.str* %str1, %struct.str* %str2) #0 {
  %1 = alloca %struct.str*, align 8
  %2 = alloca %struct.str*, align 8
  %pos = alloca i32, align 4
  store %struct.str* %str1, %struct.str** %1, align 8
  store %struct.str* %str2, %struct.str** %2, align 8
  store i32 0, i32* %pos, align 4
  %3 = load %struct.str** %2, align 8
  %4 = icmp ne %struct.str* %3, null
  br i1 %4, label %6, label %5

; <label>:5                                       ; preds = %0
  br label %49

; <label>:6                                       ; preds = %0
  %7 = load %struct.str** %2, align 8
  %8 = getelementptr inbounds %struct.str* %7, i32 0, i32 0
  %9 = load i8** %8, align 8
  %10 = icmp ne i8* %9, null
  br i1 %10, label %12, label %11

; <label>:11                                      ; preds = %6
  br label %49

; <label>:12                                      ; preds = %6
  br label %13

; <label>:13                                      ; preds = %12
  %14 = load %struct.str** %1, align 8
  %15 = icmp ne %struct.str* %14, null
  br i1 %15, label %19, label %16

; <label>:16                                      ; preds = %13
  %17 = load %struct.str** %1, align 8
  %18 = call %struct.str* @str_make(%struct.str* %17)
  store %struct.str* %18, %struct.str** %1, align 8
  br label %28

; <label>:19                                      ; preds = %13
  %20 = load %struct.str** %1, align 8
  %21 = getelementptr inbounds %struct.str* %20, i32 0, i32 0
  %22 = load i8** %21, align 8
  %23 = icmp ne i8* %22, null
  br i1 %23, label %27, label %24

; <label>:24                                      ; preds = %19
  %25 = load %struct.str** %1, align 8
  %26 = call %struct.str* @str_make(%struct.str* %25)
  store %struct.str* %26, %struct.str** %1, align 8
  br label %27

; <label>:27                                      ; preds = %24, %19
  br label %28

; <label>:28                                      ; preds = %27, %16
  br label %29

; <label>:29                                      ; preds = %28, %37
  br label %30

; <label>:30                                      ; preds = %29
  %31 = load i32* %pos, align 4
  %32 = load %struct.str** %2, align 8
  %33 = getelementptr inbounds %struct.str* %32, i32 0, i32 1
  %34 = load i32* %33, align 4
  %35 = icmp slt i32 %31, %34
  br i1 %35, label %37, label %36

; <label>:36                                      ; preds = %30
  br label %48

; <label>:37                                      ; preds = %30
  %38 = load %struct.str** %1, align 8
  %39 = load %struct.str** %2, align 8
  %40 = getelementptr inbounds %struct.str* %39, i32 0, i32 0
  %41 = load i8** %40, align 8
  %42 = load i32* %pos, align 4
  %43 = sext i32 %42 to i64
  %44 = getelementptr inbounds i8* %41, i64 %43
  %45 = load i8* %44, align 1
  call void @str_add_char(%struct.str* %38, i8 signext %45)
  %46 = load i32* %pos, align 4
  %47 = add nsw i32 %46, 1
  store i32 %47, i32* %pos, align 4
  br label %29

; <label>:48                                      ; preds = %36
  br label %49

; <label>:49                                      ; preds = %48, %11, %5
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @str_add_line(%struct.str* %str, %struct._IO_FILE* %stream) #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.str*, align 8
  %3 = alloca %struct._IO_FILE*, align 8
  %c = alloca i8, align 1
  %tmp = alloca i32, align 4
  %tmp___0 = alloca i32, align 4
  store %struct.str* %str, %struct.str** %2, align 8
  store %struct._IO_FILE* %stream, %struct._IO_FILE** %3, align 8
  %4 = load %struct.str** %2, align 8
  %5 = icmp ne %struct.str* %4, null
  br i1 %5, label %9, label %6

; <label>:6                                       ; preds = %0
  %7 = load %struct.str** %2, align 8
  %8 = call %struct.str* @str_make(%struct.str* %7)
  store %struct.str* %8, %struct.str** %2, align 8
  br label %18

; <label>:9                                       ; preds = %0
  %10 = load %struct.str** %2, align 8
  %11 = getelementptr inbounds %struct.str* %10, i32 0, i32 0
  %12 = load i8** %11, align 8
  %13 = icmp ne i8* %12, null
  br i1 %13, label %17, label %14

; <label>:14                                      ; preds = %9
  %15 = load %struct.str** %2, align 8
  %16 = call %struct.str* @str_make(%struct.str* %15)
  store %struct.str* %16, %struct.str** %2, align 8
  br label %17

; <label>:17                                      ; preds = %14, %9
  br label %18

; <label>:18                                      ; preds = %17, %6
  %19 = load %struct._IO_FILE** %3, align 8
  %20 = icmp ne %struct._IO_FILE* %19, null
  br i1 %20, label %22, label %21

; <label>:21                                      ; preds = %18
  store i32 1, i32* %1
  br label %49

; <label>:22                                      ; preds = %18
  br label %23

; <label>:23                                      ; preds = %22, %47
  br label %24

; <label>:24                                      ; preds = %23
  %25 = load %struct._IO_FILE** %3, align 8
  %26 = call i32 @_IO_getc(%struct._IO_FILE* %25)
  store i32 %26, i32* %tmp, align 4
  %27 = load i32* %tmp, align 4
  %28 = trunc i32 %27 to i8
  store i8 %28, i8* %c, align 1
  %29 = load i8* %c, align 1
  %30 = sext i8 %29 to i32
  %31 = icmp eq i32 %30, -1
  br i1 %31, label %32, label %33

; <label>:32                                      ; preds = %24
  store i32 2, i32* %1
  br label %49

; <label>:33                                      ; preds = %24
  %34 = load %struct._IO_FILE** %3, align 8
  %35 = call i32 @ferror(%struct._IO_FILE* %34)
  store i32 %35, i32* %tmp___0, align 4
  %36 = load i32* %tmp___0, align 4
  %37 = icmp ne i32 %36, 0
  br i1 %37, label %38, label %39

; <label>:38                                      ; preds = %33
  store i32 2, i32* %1
  br label %49

; <label>:39                                      ; preds = %33
  br label %40

; <label>:40                                      ; preds = %39
  %41 = load %struct.str** %2, align 8
  %42 = load i8* %c, align 1
  call void @str_add_char(%struct.str* %41, i8 signext %42)
  %43 = load i8* %c, align 1
  %44 = sext i8 %43 to i32
  %45 = icmp eq i32 %44, 10
  br i1 %45, label %46, label %47

; <label>:46                                      ; preds = %40
  br label %48

; <label>:47                                      ; preds = %40
  br label %23

; <label>:48                                      ; preds = %46
  store i32 0, i32* %1
  br label %49

; <label>:49                                      ; preds = %48, %38, %32, %21
  %50 = load i32* %1
  ret i32 %50
}

declare i32 @_IO_getc(%struct._IO_FILE*) #1

declare i32 @ferror(%struct._IO_FILE*) #1

; Function Attrs: nounwind uwtable
define i32 @str_add_line_from_desc(%struct.str* %str, i32 %file_desc) #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.str*, align 8
  %3 = alloca i32, align 4
  %nchars = alloca i32, align 4
  %c = alloca i8, align 1
  store %struct.str* %str, %struct.str** %2, align 8
  store i32 %file_desc, i32* %3, align 4
  store i32 0, i32* %nchars, align 4
  store i8 0, i8* %c, align 1
  %4 = load %struct.str** %2, align 8
  %5 = icmp ne %struct.str* %4, null
  br i1 %5, label %9, label %6

; <label>:6                                       ; preds = %0
  %7 = load %struct.str** %2, align 8
  %8 = call %struct.str* @str_make(%struct.str* %7)
  store %struct.str* %8, %struct.str** %2, align 8
  br label %18

; <label>:9                                       ; preds = %0
  %10 = load %struct.str** %2, align 8
  %11 = getelementptr inbounds %struct.str* %10, i32 0, i32 0
  %12 = load i8** %11, align 8
  %13 = icmp ne i8* %12, null
  br i1 %13, label %17, label %14

; <label>:14                                      ; preds = %9
  %15 = load %struct.str** %2, align 8
  %16 = call %struct.str* @str_make(%struct.str* %15)
  store %struct.str* %16, %struct.str** %2, align 8
  br label %17

; <label>:17                                      ; preds = %14, %9
  br label %18

; <label>:18                                      ; preds = %17, %6
  br label %19

; <label>:19                                      ; preds = %18, %37
  br label %20

; <label>:20                                      ; preds = %19
  %21 = load i32* %3, align 4
  %22 = call i32 @safe_read(i32 %21, i8* %c, i32 1)
  store i32 %22, i32* %nchars, align 4
  %23 = load i32* %nchars, align 4
  %24 = icmp ne i32 %23, 0
  br i1 %24, label %26, label %25

; <label>:25                                      ; preds = %20
  store i32 2, i32* %1
  br label %39

; <label>:26                                      ; preds = %20
  %27 = load i32* %nchars, align 4
  %28 = icmp slt i32 %27, 0
  br i1 %28, label %29, label %30

; <label>:29                                      ; preds = %26
  store i32 1, i32* %1
  br label %39

; <label>:30                                      ; preds = %26
  %31 = load %struct.str** %2, align 8
  %32 = load i8* %c, align 1
  call void @str_add_char(%struct.str* %31, i8 signext %32)
  %33 = load i8* %c, align 1
  %34 = sext i8 %33 to i32
  %35 = icmp eq i32 %34, 10
  br i1 %35, label %36, label %37

; <label>:36                                      ; preds = %30
  br label %38

; <label>:37                                      ; preds = %30
  br label %19

; <label>:38                                      ; preds = %36
  store i32 0, i32* %1
  br label %39

; <label>:39                                      ; preds = %38, %29, %25
  %40 = load i32* %1
  ret i32 %40
}

; Function Attrs: nounwind uwtable
define internal i32 @safe_read(i32 %desc, i8* %ptr, i32 %len) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8*, align 8
  %4 = alloca i32, align 4
  %n_chars = alloca i32, align 4
  %tmp = alloca i32*, align 8
  store i32 %desc, i32* %2, align 4
  store i8* %ptr, i8** %3, align 8
  store i32 %len, i32* %4, align 4
  %5 = load i32* %4, align 4
  %6 = icmp sle i32 %5, 0
  br i1 %6, label %7, label %9

; <label>:7                                       ; preds = %0
  %8 = load i32* %4, align 4
  store i32 %8, i32* %1
  br label %29

; <label>:9                                       ; preds = %0
  br label %10

; <label>:10                                      ; preds = %9, %26
  br label %11

; <label>:11                                      ; preds = %10
  %12 = load i32* %2, align 4
  %13 = load i8** %3, align 8
  %14 = load i32* %4, align 4
  %15 = call i32 (i32, i8*, i32, ...)* bitcast (i32 (...)* @read to i32 (i32, i8*, i32, ...)*)(i32 %12, i8* %13, i32 %14)
  store i32 %15, i32* %n_chars, align 4
  %16 = load i32* %n_chars, align 4
  %17 = icmp slt i32 %16, 0
  br i1 %17, label %18, label %25

; <label>:18                                      ; preds = %11
  %19 = call i32* @__errno_location() #7
  store i32* %19, i32** %tmp, align 8
  %20 = load i32** %tmp, align 8
  %21 = load i32* %20, align 4
  %22 = icmp eq i32 %21, 4
  br i1 %22, label %24, label %23

; <label>:23                                      ; preds = %18
  br label %27

; <label>:24                                      ; preds = %18
  br label %26

; <label>:25                                      ; preds = %11
  br label %27

; <label>:26                                      ; preds = %24
  br label %10

; <label>:27                                      ; preds = %25, %23
  %28 = load i32* %n_chars, align 4
  store i32 %28, i32* %1
  br label %29

; <label>:29                                      ; preds = %27, %7
  %30 = load i32* %1
  ret i32 %30
}

; Function Attrs: nounwind uwtable
define %struct.str* @nstr_to_str(i8* %nstr) #0 {
  %1 = alloca %struct.str*, align 8
  %2 = alloca i8*, align 8
  %str = alloca %struct.str*, align 8
  %pos = alloca i32, align 4
  store i8* %nstr, i8** %2, align 8
  store %struct.str* null, %struct.str** %str, align 8
  store i32 0, i32* %pos, align 4
  %3 = load %struct.str** %str, align 8
  %4 = call %struct.str* @str_make(%struct.str* %3)
  store %struct.str* %4, %struct.str** %str, align 8
  %5 = load i8** %2, align 8
  %6 = icmp ne i8* %5, null
  br i1 %6, label %9, label %7

; <label>:7                                       ; preds = %0
  %8 = load %struct.str** %str, align 8
  store %struct.str* %8, %struct.str** %1
  br label %30

; <label>:9                                       ; preds = %0
  br label %10

; <label>:10                                      ; preds = %9, %19
  br label %11

; <label>:11                                      ; preds = %10
  %12 = load i8** %2, align 8
  %13 = load i32* %pos, align 4
  %14 = sext i32 %13 to i64
  %15 = getelementptr inbounds i8* %12, i64 %14
  %16 = load i8* %15, align 1
  %17 = icmp ne i8 %16, 0
  br i1 %17, label %19, label %18

; <label>:18                                      ; preds = %11
  br label %28

; <label>:19                                      ; preds = %11
  %20 = load %struct.str** %str, align 8
  %21 = load i8** %2, align 8
  %22 = load i32* %pos, align 4
  %23 = sext i32 %22 to i64
  %24 = getelementptr inbounds i8* %21, i64 %23
  %25 = load i8* %24, align 1
  call void @str_add_char(%struct.str* %20, i8 signext %25)
  %26 = load i32* %pos, align 4
  %27 = add nsw i32 %26, 1
  store i32 %27, i32* %pos, align 4
  br label %10

; <label>:28                                      ; preds = %18
  %29 = load %struct.str** %str, align 8
  store %struct.str* %29, %struct.str** %1
  br label %30

; <label>:30                                      ; preds = %28, %7
  %31 = load %struct.str** %1
  ret %struct.str* %31
}

; Function Attrs: nounwind uwtable
define i8* @str_to_nstr(%struct.str* %str) #0 {
  %1 = alloca i8*, align 8
  %2 = alloca %struct.str*, align 8
  %nstr = alloca i8*, align 8
  %tmp = alloca i8*, align 8
  %pos = alloca i32, align 4
  store %struct.str* %str, %struct.str** %2, align 8
  %3 = load %struct.str** %2, align 8
  %4 = getelementptr inbounds %struct.str* %3, i32 0, i32 1
  %5 = load i32* %4, align 4
  %6 = add nsw i32 %5, 1
  %7 = call i8* @xmalloc(i32 %6)
  store i8* %7, i8** %tmp, align 8
  %8 = load i8** %tmp, align 8
  store i8* %8, i8** %nstr, align 8
  store i32 0, i32* %pos, align 4
  %9 = load %struct.str** %2, align 8
  %10 = icmp ne %struct.str* %9, null
  br i1 %10, label %13, label %11

; <label>:11                                      ; preds = %0
  %12 = load i8** %nstr, align 8
  store i8* %12, i8** %1
  br label %67

; <label>:13                                      ; preds = %0
  %14 = load %struct.str** %2, align 8
  %15 = getelementptr inbounds %struct.str* %14, i32 0, i32 0
  %16 = load i8** %15, align 8
  %17 = icmp ne i8* %16, null
  br i1 %17, label %20, label %18

; <label>:18                                      ; preds = %13
  %19 = load i8** %nstr, align 8
  store i8* %19, i8** %1
  br label %67

; <label>:20                                      ; preds = %13
  br label %21

; <label>:21                                      ; preds = %20
  br label %22

; <label>:22                                      ; preds = %21, %46
  br label %23

; <label>:23                                      ; preds = %22
  %24 = load i32* %pos, align 4
  %25 = load %struct.str** %2, align 8
  %26 = getelementptr inbounds %struct.str* %25, i32 0, i32 1
  %27 = load i32* %26, align 4
  %28 = icmp slt i32 %24, %27
  br i1 %28, label %30, label %29

; <label>:29                                      ; preds = %23
  br label %60

; <label>:30                                      ; preds = %23
  %31 = load %struct.str** %2, align 8
  %32 = getelementptr inbounds %struct.str* %31, i32 0, i32 0
  %33 = load i8** %32, align 8
  %34 = load i32* %pos, align 4
  %35 = sext i32 %34 to i64
  %36 = getelementptr inbounds i8* %33, i64 %35
  %37 = load i8* %36, align 1
  %38 = icmp ne i8 %37, 0
  br i1 %38, label %46, label %39

; <label>:39                                      ; preds = %30
  %40 = load %struct.str** %2, align 8
  %41 = getelementptr inbounds %struct.str* %40, i32 0, i32 0
  %42 = load i8** %41, align 8
  %43 = load i32* %pos, align 4
  %44 = sext i32 %43 to i64
  %45 = getelementptr inbounds i8* %42, i64 %44
  store i8 32, i8* %45, align 1
  br label %46

; <label>:46                                      ; preds = %39, %30
  %47 = load %struct.str** %2, align 8
  %48 = getelementptr inbounds %struct.str* %47, i32 0, i32 0
  %49 = load i8** %48, align 8
  %50 = load i32* %pos, align 4
  %51 = sext i32 %50 to i64
  %52 = getelementptr inbounds i8* %49, i64 %51
  %53 = load i8* %52, align 1
  %54 = load i8** %nstr, align 8
  %55 = load i32* %pos, align 4
  %56 = sext i32 %55 to i64
  %57 = getelementptr inbounds i8* %54, i64 %56
  store i8 %53, i8* %57, align 1
  %58 = load i32* %pos, align 4
  %59 = add nsw i32 %58, 1
  store i32 %59, i32* %pos, align 4
  br label %22

; <label>:60                                      ; preds = %29
  %61 = load i8** %nstr, align 8
  %62 = load i32* %pos, align 4
  %63 = add nsw i32 %62, 1
  %64 = sext i32 %63 to i64
  %65 = getelementptr inbounds i8* %61, i64 %64
  store i8 0, i8* %65, align 1
  %66 = load i8** %nstr, align 8
  store i8* %66, i8** %1
  br label %67

; <label>:67                                      ; preds = %60, %18, %11
  %68 = load i8** %1
  ret i8* %68
}

; Function Attrs: nounwind uwtable
define %struct.str* @int_to_str(i32 %num) #0 {
  %1 = alloca i32, align 4
  %c = alloca [2 x i8], align 1
  %pos = alloca i32, align 4
  %rem = alloca i32, align 4
  %rstr = alloca %struct.str*, align 8
  %tmp = alloca %struct.str*, align 8
  %str = alloca %struct.str*, align 8
  %tmp___0 = alloca %struct.str*, align 8
  store i32 %num, i32* %1, align 4
  store i32 0, i32* %pos, align 4
  store i32 0, i32* %rem, align 4
  %2 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %2, %struct.str** %tmp, align 8
  %3 = load %struct.str** %tmp, align 8
  store %struct.str* %3, %struct.str** %rstr, align 8
  %4 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %4, %struct.str** %tmp___0, align 8
  %5 = load %struct.str** %tmp___0, align 8
  store %struct.str* %5, %struct.str** %str, align 8
  br label %6

; <label>:6                                       ; preds = %0, %11
  br label %7

; <label>:7                                       ; preds = %6
  %8 = load i32* %1, align 4
  %9 = icmp sgt i32 %8, 0
  br i1 %9, label %11, label %10

; <label>:10                                      ; preds = %7
  br label %22

; <label>:11                                      ; preds = %7
  %12 = load i32* %1, align 4
  %13 = srem i32 %12, 10
  store i32 %13, i32* %rem, align 4
  %14 = load i32* %1, align 4
  %15 = sdiv i32 %14, 10
  store i32 %15, i32* %1, align 4
  %16 = getelementptr inbounds [2 x i8]* %c, i32 0, i32 0
  %17 = load i32* %rem, align 4
  %18 = call i32 (i8*, i8*, ...)* @sprintf(i8* %16, i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %17)
  %19 = load %struct.str** %rstr, align 8
  %20 = getelementptr inbounds [2 x i8]* %c, i32 0, i64 0
  %21 = load i8* %20, align 1
  call void @str_add_char(%struct.str* %19, i8 signext %21)
  br label %6

; <label>:22                                      ; preds = %10
  %23 = load %struct.str** %rstr, align 8
  %24 = getelementptr inbounds %struct.str* %23, i32 0, i32 1
  %25 = load i32* %24, align 4
  %26 = sub nsw i32 %25, 1
  store i32 %26, i32* %pos, align 4
  br label %27

; <label>:27                                      ; preds = %22, %32
  br label %28

; <label>:28                                      ; preds = %27
  %29 = load i32* %pos, align 4
  %30 = icmp sge i32 %29, 0
  br i1 %30, label %32, label %31

; <label>:31                                      ; preds = %28
  br label %43

; <label>:32                                      ; preds = %28
  %33 = load %struct.str** %str, align 8
  %34 = load %struct.str** %rstr, align 8
  %35 = getelementptr inbounds %struct.str* %34, i32 0, i32 0
  %36 = load i8** %35, align 8
  %37 = load i32* %pos, align 4
  %38 = sext i32 %37 to i64
  %39 = getelementptr inbounds i8* %36, i64 %38
  %40 = load i8* %39, align 1
  call void @str_add_char(%struct.str* %33, i8 signext %40)
  %41 = load i32* %pos, align 4
  %42 = add nsw i32 %41, -1
  store i32 %42, i32* %pos, align 4
  br label %27

; <label>:43                                      ; preds = %31
  %44 = load %struct.str** %str, align 8
  ret %struct.str* %44
}

declare i32 @sprintf(i8*, i8*, ...) #1

; Function Attrs: inlinehint nounwind uwtable
define i32 @stat(i8* noalias %__path, %struct.stat* noalias %__statbuf) #2 {
  %1 = alloca i8*, align 8
  %2 = alloca %struct.stat*, align 8
  %tmp = alloca i32, align 4
  store i8* %__path, i8** %1, align 8
  store %struct.stat* %__statbuf, %struct.stat** %2, align 8
  %3 = load i8** %1, align 8
  %4 = load %struct.stat** %2, align 8
  %5 = call i32 @__xstat(i32 3, i8* %3, %struct.stat* %4)
  store i32 %5, i32* %tmp, align 4
  %6 = load i32* %tmp, align 4
  ret i32 %6
}

declare i32 @__xstat(i32, i8*, %struct.stat*) #1

; Function Attrs: inlinehint nounwind uwtable
define i32 @lstat(i8* noalias %__path, %struct.stat* noalias %__statbuf) #2 {
  %1 = alloca i8*, align 8
  %2 = alloca %struct.stat*, align 8
  %tmp = alloca i32, align 4
  store i8* %__path, i8** %1, align 8
  store %struct.stat* %__statbuf, %struct.stat** %2, align 8
  %3 = load i8** %1, align 8
  %4 = load %struct.stat** %2, align 8
  %5 = call i32 @__lxstat(i32 3, i8* %3, %struct.stat* %4)
  store i32 %5, i32* %tmp, align 4
  %6 = load i32* %tmp, align 4
  ret i32 %6
}

declare i32 @__lxstat(i32, i8*, %struct.stat*) #1

; Function Attrs: inlinehint nounwind uwtable
define i32 @fstat(i32 %__fd, %struct.stat* %__statbuf) #2 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.stat*, align 8
  %tmp = alloca i32, align 4
  store i32 %__fd, i32* %1, align 4
  store %struct.stat* %__statbuf, %struct.stat** %2, align 8
  %3 = load i32* %1, align 4
  %4 = load %struct.stat** %2, align 8
  %5 = call i32 @__fxstat(i32 3, i32 %3, %struct.stat* %4)
  store i32 %5, i32* %tmp, align 4
  %6 = load i32* %tmp, align 4
  ret i32 %6
}

declare i32 @__fxstat(i32, i32, %struct.stat*) #1

; Function Attrs: inlinehint nounwind uwtable
define i32 @mknod(i8* %__path, i32 %__mode, i64 %__dev) #2 {
  %1 = alloca i8*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i64, align 8
  %tmp = alloca i32, align 4
  store i8* %__path, i8** %1, align 8
  store i32 %__mode, i32* %2, align 4
  store i64 %__dev, i64* %3, align 8
  %4 = load i8** %1, align 8
  %5 = load i32* %2, align 4
  %6 = call i32 @__xmknod(i32 1, i8* %4, i32 %5, i64* %3)
  store i32 %6, i32* %tmp, align 4
  %7 = load i32* %tmp, align 4
  ret i32 %7
}

declare i32 @__xmknod(i32, i8*, i32, i64*) #1

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %opt = alloca i8, align 1
  %opt_error = alloca i32, align 4
  %show_help = alloca i32, align 4
  %show_version = alloca i32, align 4
  %pid = alloca i32, align 4
  %ispell_pipe = alloca %struct.pipe, align 8
  %tmp = alloca i32, align 4
  %tmp___0 = alloca i32*, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  store i8 0, i8* %opt, align 1
  store i32 0, i32* %opt_error, align 4
  store i32 0, i32* %show_help, align 4
  store i32 0, i32* %show_version, align 4
  store i32 0, i32* %pid, align 4
  %4 = load i8*** %3, align 8
  %5 = getelementptr inbounds i8** %4, i64 0
  %6 = load i8** %5, align 8
  store i8* %6, i8** @program_name, align 8
  br label %7

; <label>:7                                       ; preds = %0, %108
  br label %8

; <label>:8                                       ; preds = %7
  %9 = load i32* %2, align 4
  %10 = load i8*** %3, align 8
  %11 = call i32 @getopt_long(i32 %9, i8** %10, i8* getelementptr inbounds ([13 x i8]* @.str13, i32 0, i32 0), %struct.option* getelementptr inbounds ([13 x %struct.option]* bitcast (<{ { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] }, { i8*, i32, i32*, i32, [4 x i8] } }>* @long_options to [13 x %struct.option]*), i32 0, i32 0), i32* null)
  store i32 %11, i32* %tmp, align 4
  %12 = load i32* %tmp, align 4
  %13 = trunc i32 %12 to i8
  store i8 %13, i8* %opt, align 1
  %14 = load i8* %opt, align 1
  %15 = sext i8 %14 to i32
  %16 = icmp eq i32 %15, -1
  br i1 %16, label %17, label %18

; <label>:17                                      ; preds = %8
  br label %109

; <label>:18                                      ; preds = %8
  %19 = load i8* %opt, align 1
  %20 = sext i8 %19 to i32
  %21 = icmp eq i32 %20, 73
  br i1 %21, label %22, label %23

; <label>:22                                      ; preds = %18
  br label %79

; <label>:23                                      ; preds = %18
  %24 = load i8* %opt, align 1
  %25 = sext i8 %24 to i32
  %26 = icmp eq i32 %25, 86
  br i1 %26, label %27, label %28

; <label>:27                                      ; preds = %23
  br label %80

; <label>:28                                      ; preds = %23
  %29 = load i8* %opt, align 1
  %30 = sext i8 %29 to i32
  %31 = icmp eq i32 %30, 98
  br i1 %31, label %32, label %33

; <label>:32                                      ; preds = %28
  br label %81

; <label>:33                                      ; preds = %28
  %34 = load i8* %opt, align 1
  %35 = sext i8 %34 to i32
  %36 = icmp eq i32 %35, 100
  br i1 %36, label %37, label %38

; <label>:37                                      ; preds = %33
  br label %82

; <label>:38                                      ; preds = %33
  %39 = load i8* %opt, align 1
  %40 = sext i8 %39 to i32
  %41 = icmp eq i32 %40, 104
  br i1 %41, label %42, label %43

; <label>:42                                      ; preds = %38
  br label %91

; <label>:43                                      ; preds = %38
  %44 = load i8* %opt, align 1
  %45 = sext i8 %44 to i32
  %46 = icmp eq i32 %45, 105
  br i1 %46, label %47, label %48

; <label>:47                                      ; preds = %43
  br label %92

; <label>:48                                      ; preds = %43
  %49 = load i8* %opt, align 1
  %50 = sext i8 %49 to i32
  %51 = icmp eq i32 %50, 108
  br i1 %51, label %52, label %53

; <label>:52                                      ; preds = %48
  br label %101

; <label>:53                                      ; preds = %48
  %54 = load i8* %opt, align 1
  %55 = sext i8 %54 to i32
  %56 = icmp eq i32 %55, 110
  br i1 %56, label %57, label %58

; <label>:57                                      ; preds = %53
  br label %102

; <label>:58                                      ; preds = %53
  %59 = load i8* %opt, align 1
  %60 = sext i8 %59 to i32
  %61 = icmp eq i32 %60, 111
  br i1 %61, label %62, label %63

; <label>:62                                      ; preds = %58
  br label %103

; <label>:63                                      ; preds = %58
  %64 = load i8* %opt, align 1
  %65 = sext i8 %64 to i32
  %66 = icmp eq i32 %65, 115
  br i1 %66, label %67, label %68

; <label>:67                                      ; preds = %63
  br label %104

; <label>:68                                      ; preds = %63
  %69 = load i8* %opt, align 1
  %70 = sext i8 %69 to i32
  %71 = icmp eq i32 %70, 118
  br i1 %71, label %72, label %73

; <label>:72                                      ; preds = %68
  br label %105

; <label>:73                                      ; preds = %68
  %74 = load i8* %opt, align 1
  %75 = sext i8 %74 to i32
  %76 = icmp eq i32 %75, 120
  br i1 %76, label %77, label %78

; <label>:77                                      ; preds = %73
  br label %106

; <label>:78                                      ; preds = %73
  br label %107

; <label>:79                                      ; preds = %22
  store i32 1, i32* @show_ispell_version, align 4
  br label %108

; <label>:80                                      ; preds = %27
  store i32 1, i32* %show_version, align 4
  br label %108

; <label>:81                                      ; preds = %32
  store i32 1, i32* @british, align 4
  br label %108

; <label>:82                                      ; preds = %37
  %83 = load i8** @optarg, align 8
  %84 = ptrtoint i8* %83 to i64
  %85 = icmp ne i64 %84, 0
  br i1 %85, label %86, label %89

; <label>:86                                      ; preds = %82
  %87 = load i8** @optarg, align 8
  %88 = call i8* @xstrdup(i8* %87)
  store i8* %88, i8** @dictionary, align 8
  br label %90

; <label>:89                                      ; preds = %82
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 0, i8* getelementptr inbounds ([26 x i8]* @.str14, i32 0, i32 0))
  br label %90

; <label>:90                                      ; preds = %89, %86
  br label %108

; <label>:91                                      ; preds = %42
  store i32 1, i32* %show_help, align 4
  br label %108

; <label>:92                                      ; preds = %47
  %93 = load i8** @optarg, align 8
  %94 = ptrtoint i8* %93 to i64
  %95 = icmp ne i64 %94, 0
  br i1 %95, label %96, label %99

; <label>:96                                      ; preds = %92
  %97 = load i8** @optarg, align 8
  %98 = call i8* @xstrdup(i8* %97)
  store i8* %98, i8** @ispell_prog, align 8
  br label %100

; <label>:99                                      ; preds = %92
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 0, i8* getelementptr inbounds ([26 x i8]* @.str14, i32 0, i32 0))
  br label %100

; <label>:100                                     ; preds = %99, %96
  br label %108

; <label>:101                                     ; preds = %52
  br label %108

; <label>:102                                     ; preds = %57
  store i32 1, i32* @number_lines, align 4
  br label %108

; <label>:103                                     ; preds = %62
  store i32 1, i32* @print_file_names, align 4
  br label %108

; <label>:104                                     ; preds = %67
  br label %108

; <label>:105                                     ; preds = %72
  store i32 1, i32* @verbose, align 4
  br label %108

; <label>:106                                     ; preds = %77
  br label %108

; <label>:107                                     ; preds = %78
  store i32 1, i32* %opt_error, align 4
  br label %108

; <label>:108                                     ; preds = %107, %106, %105, %104, %103, %102, %101, %100, %91, %90, %81, %80, %79
  br label %7

; <label>:109                                     ; preds = %17
  %110 = load i32* %opt_error, align 4
  %111 = icmp ne i32 %110, 0
  br i1 %111, label %112, label %115

; <label>:112                                     ; preds = %109
  %113 = load i8** @program_name, align 8
  %114 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([39 x i8]* @.str15, i32 0, i32 0), i8* %113)
  call void @exit(i32 1) #8
  unreachable

; <label>:115                                     ; preds = %109
  %116 = load i32* %show_version, align 4
  %117 = icmp ne i32 %116, 0
  br i1 %117, label %118, label %123

; <label>:118                                     ; preds = %115
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 0, i8* getelementptr inbounds ([12 x i8]* @version, i32 0, i32 0))
  %119 = load i32* %show_help, align 4
  %120 = icmp ne i32 %119, 0
  br i1 %120, label %122, label %121

; <label>:121                                     ; preds = %118
  call void @exit(i32 0) #8
  unreachable

; <label>:122                                     ; preds = %118
  br label %123

; <label>:123                                     ; preds = %122, %115
  %124 = load i32* %show_help, align 4
  %125 = icmp ne i32 %124, 0
  br i1 %125, label %126, label %131

; <label>:126                                     ; preds = %123
  %127 = load i8** @program_name, align 8
  %128 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([33 x i8]* @.str16, i32 0, i32 0), i8* %127)
  %129 = load %struct._IO_FILE** @stderr, align 8
  %130 = call i32 @fputs(i8* getelementptr inbounds ([687 x i8]* @.str17, i32 0, i32 0), %struct._IO_FILE* %129)
  call void @exit(i32 0) #8
  unreachable

; <label>:131                                     ; preds = %123
  %132 = load i8** @ispell_prog, align 8
  %133 = icmp ne i8* %132, null
  br i1 %133, label %136, label %134

; <label>:134                                     ; preds = %131
  %135 = call i8* @find_ispell()
  store i8* %135, i8** @ispell_prog, align 8
  br label %136

; <label>:136                                     ; preds = %134, %131
  call void @new_pipe(%struct.pipe* %ispell_pipe)
  %137 = call i32 @fork()
  store i32 %137, i32* %pid, align 4
  %138 = load i32* %pid, align 4
  %139 = icmp slt i32 %138, 0
  br i1 %139, label %140, label %144

; <label>:140                                     ; preds = %136
  %141 = call i32* @__errno_location() #7
  store i32* %141, i32** %tmp___0, align 8
  %142 = load i32** %tmp___0, align 8
  %143 = load i32* %142, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %143, i8* getelementptr inbounds ([28 x i8]* @.str18, i32 0, i32 0))
  br label %152

; <label>:144                                     ; preds = %136
  %145 = load i32* %pid, align 4
  %146 = icmp sgt i32 %145, 0
  br i1 %146, label %147, label %150

; <label>:147                                     ; preds = %144
  %148 = load i32* %2, align 4
  %149 = load i8*** %3, align 8
  call void @parent(%struct.pipe* %ispell_pipe, i32 %148, i8** %149)
  br label %151

; <label>:150                                     ; preds = %144
  call void @run_ispell_in_child(%struct.pipe* %ispell_pipe)
  br label %151

; <label>:151                                     ; preds = %150, %147
  br label %152

; <label>:152                                     ; preds = %151, %140
  call void @exit(i32 0) #8
  unreachable
                                                  ; No predecessors!
  %154 = load i32* %1
  ret i32 %154
}

declare i32 @getopt_long(i32, i8**, i8*, %struct.option*, i32*) #1

; Function Attrs: nounwind uwtable
define internal i8* @xstrdup(i8* %str) #0 {
  %1 = alloca i8*, align 8
  %len = alloca i32, align 4
  %tmp = alloca i32, align 4
  %new = alloca i8*, align 8
  %tmp___0 = alloca i8*, align 8
  store i8* %str, i8** %1, align 8
  %2 = load i8** %1, align 8
  %3 = call i32 @strlen(i8* %2) #9
  store i32 %3, i32* %tmp, align 4
  %4 = load i32* %tmp, align 4
  %5 = add i32 %4, 1
  store i32 %5, i32* %len, align 4
  %6 = load i32* %len, align 4
  %7 = call i8* @xmalloc___0(i32 %6)
  store i8* %7, i8** %tmp___0, align 8
  %8 = load i8** %tmp___0, align 8
  store i8* %8, i8** %new, align 8
  %9 = load i8** %new, align 8
  %10 = load i8** %1, align 8
  %11 = load i32* %len, align 4
  %12 = call i8* @memcpy(i8* %9, i8* %10, i32 %11)
  %13 = load i8** %new, align 8
  ret i8* %13
}

; Function Attrs: nounwind uwtable
define internal void @error___0(i32 %status, i32 %errnum, i8* %message, ...) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8*, align 8
  %args = alloca [1 x %struct.__va_list_tag], align 16
  %tmp = alloca i8*, align 8
  store i32 %status, i32* %1, align 4
  store i32 %errnum, i32* %2, align 4
  store i8* %message, i8** %3, align 8
  %4 = load %struct._IO_FILE** @stdout, align 8
  %5 = call i32 @fflush(%struct._IO_FILE* %4)
  %6 = load %struct._IO_FILE** @stderr, align 8
  %7 = load i8** @program_name, align 8
  %8 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %6, i8* getelementptr inbounds ([5 x i8]* @.str49, i32 0, i32 0), i8* %7)
  %9 = getelementptr inbounds [1 x %struct.__va_list_tag]* %args, i32 0, i32 0
  %10 = bitcast %struct.__va_list_tag* %9 to i8*
  call void @llvm.va_start(i8* %10)
  %11 = load %struct._IO_FILE** @stderr, align 8
  %12 = load i8** %3, align 8
  %13 = getelementptr inbounds [1 x %struct.__va_list_tag]* %args, i32 0, i32 0
  %14 = call i32 @vfprintf(%struct._IO_FILE* %11, i8* %12, %struct.__va_list_tag* %13)
  %15 = getelementptr inbounds [1 x %struct.__va_list_tag]* %args, i32 0, i32 0
  %16 = bitcast %struct.__va_list_tag* %15 to i8*
  call void @llvm.va_end(i8* %16)
  %17 = load i32* %2, align 4
  %18 = icmp ne i32 %17, 0
  br i1 %18, label %19, label %25

; <label>:19                                      ; preds = %0
  %20 = load i32* %2, align 4
  %21 = call i8* @strerror(i32 %20)
  store i8* %21, i8** %tmp, align 8
  %22 = load %struct._IO_FILE** @stderr, align 8
  %23 = load i8** %tmp, align 8
  %24 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %22, i8* getelementptr inbounds ([5 x i8]* @.str50, i32 0, i32 0), i8* %23)
  br label %25

; <label>:25                                      ; preds = %19, %0
  %26 = load %struct._IO_FILE** @stderr, align 8
  %27 = call i32 @_IO_putc(i32 10, %struct._IO_FILE* %26)
  %28 = load %struct._IO_FILE** @stderr, align 8
  %29 = call i32 @fflush(%struct._IO_FILE* %28)
  %30 = load i32* %1, align 4
  %31 = icmp ne i32 %30, 0
  br i1 %31, label %32, label %34

; <label>:32                                      ; preds = %25
  %33 = load i32* %1, align 4
  call void @exit(i32 %33) #8
  unreachable

; <label>:34                                      ; preds = %25
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noreturn
declare void @exit(i32) #3

declare i32 @fputs(i8*, %struct._IO_FILE*) #1

; Function Attrs: nounwind uwtable
define i8* @find_ispell() #0 {
  %ispell = alloca i8*, align 8
  %path = alloca i8*, align 8
  %path_len = alloca i32, align 4
  %pos = alloca i32, align 4
  %file = alloca %struct.str*, align 8
  %tmp = alloca %struct.str*, align 8
  %stat_buf = alloca %struct.stat, align 8
  %tmp___0 = alloca i8*, align 8
  %tmp___1 = alloca i32, align 4
  %tmp___2 = alloca %struct.str*, align 8
  %tmp___3 = alloca i8*, align 8
  %tmp___4 = alloca i8*, align 8
  %tmp___5 = alloca i8*, align 8
  %tmp___6 = alloca i32, align 4
  store i8* null, i8** %ispell, align 8
  store i8* null, i8** %path, align 8
  store i32 0, i32* %path_len, align 4
  store i32 0, i32* %pos, align 4
  %1 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %1, %struct.str** %tmp, align 8
  %2 = load %struct.str** %tmp, align 8
  store %struct.str* %2, %struct.str** %file, align 8
  %3 = call i8* @getenv(i8* getelementptr inbounds ([5 x i8]* @.str19, i32 0, i32 0))
  store i8* %3, i8** %tmp___0, align 8
  %4 = load i8** %tmp___0, align 8
  %5 = call i8* @xstrdup(i8* %4)
  store i8* %5, i8** %path, align 8
  %6 = load i8** %path, align 8
  %7 = call i32 @strlen(i8* %6) #9
  store i32 %7, i32* %tmp___1, align 4
  %8 = load i32* %tmp___1, align 4
  store i32 %8, i32* %path_len, align 4
  br label %9

; <label>:9                                       ; preds = %0, %91
  br label %10

; <label>:10                                      ; preds = %9
  %11 = load %struct.str** %file, align 8
  %12 = call %struct.str* @str_make(%struct.str* %11)
  store %struct.str* %12, %struct.str** %file, align 8
  %13 = load i32* %pos, align 4
  %14 = icmp ne i32 %13, 0
  br i1 %14, label %27, label %15

; <label>:15                                      ; preds = %10
  %16 = load i8** %path, align 8
  %17 = load i32* %pos, align 4
  %18 = sext i32 %17 to i64
  %19 = getelementptr inbounds i8* %16, i64 %18
  %20 = load i8* %19, align 1
  %21 = sext i8 %20 to i32
  %22 = icmp eq i32 %21, 58
  br i1 %22, label %23, label %25

; <label>:23                                      ; preds = %15
  %24 = load %struct.str** %file, align 8
  call void @str_add_char(%struct.str* %24, i8 signext 46)
  br label %26

; <label>:25                                      ; preds = %15
  br label %28

; <label>:26                                      ; preds = %23
  br label %55

; <label>:27                                      ; preds = %10
  br label %28

; <label>:28                                      ; preds = %27, %25
  br label %29

; <label>:29                                      ; preds = %28, %45
  br label %30

; <label>:30                                      ; preds = %29
  %31 = load i32* %pos, align 4
  %32 = load i32* %path_len, align 4
  %33 = icmp slt i32 %31, %32
  br i1 %33, label %34, label %44

; <label>:34                                      ; preds = %30
  %35 = load i8** %path, align 8
  %36 = load i32* %pos, align 4
  %37 = sext i32 %36 to i64
  %38 = getelementptr inbounds i8* %35, i64 %37
  %39 = load i8* %38, align 1
  %40 = sext i8 %39 to i32
  %41 = icmp ne i32 %40, 58
  br i1 %41, label %43, label %42

; <label>:42                                      ; preds = %34
  br label %54

; <label>:43                                      ; preds = %34
  br label %45

; <label>:44                                      ; preds = %30
  br label %54

; <label>:45                                      ; preds = %43
  %46 = load %struct.str** %file, align 8
  %47 = load i8** %path, align 8
  %48 = load i32* %pos, align 4
  %49 = sext i32 %48 to i64
  %50 = getelementptr inbounds i8* %47, i64 %49
  %51 = load i8* %50, align 1
  call void @str_add_char(%struct.str* %46, i8 signext %51)
  %52 = load i32* %pos, align 4
  %53 = add nsw i32 %52, 1
  store i32 %53, i32* %pos, align 4
  br label %29

; <label>:54                                      ; preds = %44, %42
  br label %55

; <label>:55                                      ; preds = %54, %26
  %56 = load %struct.str** %file, align 8
  %57 = getelementptr inbounds %struct.str* %56, i32 0, i32 0
  %58 = load i8** %57, align 8
  %59 = load %struct.str** %file, align 8
  %60 = getelementptr inbounds %struct.str* %59, i32 0, i32 1
  %61 = load i32* %60, align 4
  %62 = sub nsw i32 %61, 1
  %63 = sext i32 %62 to i64
  %64 = getelementptr inbounds i8* %58, i64 %63
  %65 = load i8* %64, align 1
  %66 = sext i8 %65 to i32
  %67 = icmp ne i32 %66, 47
  br i1 %67, label %68, label %70

; <label>:68                                      ; preds = %55
  %69 = load %struct.str** %file, align 8
  call void @str_add_char(%struct.str* %69, i8 signext 47)
  br label %70

; <label>:70                                      ; preds = %68, %55
  %71 = call %struct.str* @nstr_to_str(i8* getelementptr inbounds ([7 x i8]* @.str5, i32 0, i32 0))
  store %struct.str* %71, %struct.str** %tmp___2, align 8
  %72 = load %struct.str** %file, align 8
  %73 = load %struct.str** %tmp___2, align 8
  call void @str_add_str(%struct.str* %72, %struct.str* %73)
  %74 = load %struct.str** %file, align 8
  %75 = call i8* @str_to_nstr(%struct.str* %74)
  store i8* %75, i8** %tmp___5, align 8
  %76 = load i8** %tmp___5, align 8
  %77 = call i32 @stat(i8* %76, %struct.stat* %stat_buf)
  store i32 %77, i32* %tmp___6, align 4
  %78 = load i32* %tmp___6, align 4
  %79 = icmp ne i32 %78, -1
  br i1 %79, label %80, label %86

; <label>:80                                      ; preds = %70
  %81 = load %struct.str** %file, align 8
  %82 = call i8* @str_to_nstr(%struct.str* %81)
  store i8* %82, i8** %tmp___3, align 8
  %83 = load i8** %tmp___3, align 8
  %84 = call i8* @xstrdup(i8* %83)
  store i8* %84, i8** %tmp___4, align 8
  %85 = load i8** %tmp___4, align 8
  ret i8* %85

; <label>:86                                      ; preds = %70
  %87 = load i32* %pos, align 4
  %88 = load i32* %path_len, align 4
  %89 = icmp sge i32 %87, %88
  br i1 %89, label %90, label %91

; <label>:90                                      ; preds = %86
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([24 x i8]* @.str20, i32 0, i32 0))
  br label %91

; <label>:91                                      ; preds = %90, %86
  %92 = load i32* %pos, align 4
  %93 = add nsw i32 %92, 1
  store i32 %93, i32* %pos, align 4
  br label %9
                                                  ; No predecessors!
  call void @abort() #8
  unreachable
}

; Function Attrs: nounwind uwtable
define void @new_pipe(%struct.pipe* %the_pipe) #0 {
  %1 = alloca %struct.pipe*, align 8
  %ifd = alloca [2 x i32], align 4
  %ofd = alloca [2 x i32], align 4
  %efd = alloca [2 x i32], align 4
  %tmp = alloca i32*, align 8
  %tmp___0 = alloca void (i32)*, align 8
  %tmp___1 = alloca i32*, align 8
  %tmp___2 = alloca void (i32)*, align 8
  %tmp___3 = alloca i32*, align 8
  %tmp___4 = alloca i32, align 4
  %tmp___5 = alloca i32*, align 8
  %tmp___6 = alloca i32, align 4
  %tmp___7 = alloca i32*, align 8
  %tmp___8 = alloca i32, align 4
  store %struct.pipe* %the_pipe, %struct.pipe** %1, align 8
  %2 = call void (i32)* (i32, void (i32)*)* @signal(i32 13, void (i32)* @sig_pipe)
  store void (i32)* %2, void (i32)** %tmp___0, align 8
  %3 = load void (i32)** %tmp___0, align 8
  %4 = ptrtoint void (i32)* %3 to i64
  %5 = icmp eq i64 %4, -1
  br i1 %5, label %6, label %10

; <label>:6                                       ; preds = %0
  %7 = call i32* @__errno_location() #7
  store i32* %7, i32** %tmp, align 8
  %8 = load i32** %tmp, align 8
  %9 = load i32* %8, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %9, i8* getelementptr inbounds ([31 x i8]* @.str31, i32 0, i32 0))
  br label %10

; <label>:10                                      ; preds = %6, %0
  %11 = call void (i32)* (i32, void (i32)*)* @signal(i32 17, void (i32)* @sig_chld)
  store void (i32)* %11, void (i32)** %tmp___2, align 8
  %12 = load void (i32)** %tmp___2, align 8
  %13 = ptrtoint void (i32)* %12 to i64
  %14 = icmp eq i64 %13, -1
  br i1 %14, label %15, label %19

; <label>:15                                      ; preds = %10
  %16 = call i32* @__errno_location() #7
  store i32* %16, i32** %tmp___1, align 8
  %17 = load i32** %tmp___1, align 8
  %18 = load i32* %17, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %18, i8* getelementptr inbounds ([31 x i8]* @.str32, i32 0, i32 0))
  br label %19

; <label>:19                                      ; preds = %15, %10
  %20 = getelementptr inbounds [2 x i32]* %ifd, i32 0, i32 0
  %21 = call i32 @pipe(i32* %20)
  store i32 %21, i32* %tmp___4, align 4
  %22 = load i32* %tmp___4, align 4
  %23 = icmp slt i32 %22, 0
  br i1 %23, label %24, label %28

; <label>:24                                      ; preds = %19
  %25 = call i32* @__errno_location() #7
  store i32* %25, i32** %tmp___3, align 8
  %26 = load i32** %tmp___3, align 8
  %27 = load i32* %26, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %27, i8* getelementptr inbounds ([30 x i8]* @.str33, i32 0, i32 0))
  br label %28

; <label>:28                                      ; preds = %24, %19
  %29 = getelementptr inbounds [2 x i32]* %ifd, i32 0, i64 0
  %30 = load i32* %29, align 4
  %31 = load %struct.pipe** %1, align 8
  %32 = getelementptr inbounds %struct.pipe* %31, i32 0, i32 0
  store i32 %30, i32* %32, align 4
  %33 = getelementptr inbounds [2 x i32]* %ifd, i32 0, i64 1
  %34 = load i32* %33, align 4
  %35 = load %struct.pipe** %1, align 8
  %36 = getelementptr inbounds %struct.pipe* %35, i32 0, i32 4
  store i32 %34, i32* %36, align 4
  %37 = getelementptr inbounds [2 x i32]* %ofd, i32 0, i32 0
  %38 = call i32 @pipe(i32* %37)
  store i32 %38, i32* %tmp___6, align 4
  %39 = load i32* %tmp___6, align 4
  %40 = icmp slt i32 %39, 0
  br i1 %40, label %41, label %45

; <label>:41                                      ; preds = %28
  %42 = call i32* @__errno_location() #7
  store i32* %42, i32** %tmp___5, align 8
  %43 = load i32** %tmp___5, align 8
  %44 = load i32* %43, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %44, i8* getelementptr inbounds ([30 x i8]* @.str33, i32 0, i32 0))
  br label %45

; <label>:45                                      ; preds = %41, %28
  %46 = getelementptr inbounds [2 x i32]* %ofd, i32 0, i64 0
  %47 = load i32* %46, align 4
  %48 = load %struct.pipe** %1, align 8
  %49 = getelementptr inbounds %struct.pipe* %48, i32 0, i32 3
  store i32 %47, i32* %49, align 4
  %50 = getelementptr inbounds [2 x i32]* %ofd, i32 0, i64 1
  %51 = load i32* %50, align 4
  %52 = load %struct.pipe** %1, align 8
  %53 = getelementptr inbounds %struct.pipe* %52, i32 0, i32 1
  store i32 %51, i32* %53, align 4
  %54 = getelementptr inbounds [2 x i32]* %efd, i32 0, i32 0
  %55 = call i32 @pipe(i32* %54)
  store i32 %55, i32* %tmp___8, align 4
  %56 = load i32* %tmp___8, align 4
  %57 = icmp slt i32 %56, 0
  br i1 %57, label %58, label %62

; <label>:58                                      ; preds = %45
  %59 = call i32* @__errno_location() #7
  store i32* %59, i32** %tmp___7, align 8
  %60 = load i32** %tmp___7, align 8
  %61 = load i32* %60, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %61, i8* getelementptr inbounds ([30 x i8]* @.str33, i32 0, i32 0))
  br label %62

; <label>:62                                      ; preds = %58, %45
  %63 = getelementptr inbounds [2 x i32]* %efd, i32 0, i64 0
  %64 = load i32* %63, align 4
  %65 = load %struct.pipe** %1, align 8
  %66 = getelementptr inbounds %struct.pipe* %65, i32 0, i32 2
  store i32 %64, i32* %66, align 4
  %67 = getelementptr inbounds [2 x i32]* %efd, i32 0, i64 1
  %68 = load i32* %67, align 4
  %69 = load %struct.pipe** %1, align 8
  %70 = getelementptr inbounds %struct.pipe* %69, i32 0, i32 5
  store i32 %68, i32* %70, align 4
  ret void
}

declare i32 @fork() #1

; Function Attrs: nounwind readnone
declare i32* @__errno_location() #4

; Function Attrs: nounwind uwtable
define void @parent(%struct.pipe* %the_pipe, i32 %argc, i8** %argv) #0 {
  %1 = alloca %struct.pipe*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %stream = alloca %struct._IO_FILE*, align 8
  %file = alloca i8*, align 8
  %arg_error = alloca i32, align 4
  %arg_index = alloca i32, align 4
  %pos = alloca i32, align 4
  %ispell_version = alloca %struct.str*, align 8
  %tmp = alloca %struct.str*, align 8
  %str = alloca %struct.str*, align 8
  %tmp___0 = alloca %struct.str*, align 8
  %tmp___1 = alloca i32, align 4
  %tmp___2 = alloca i16**, align 8
  %tmp___3 = alloca i8*, align 8
  %stat_buf = alloca %struct.stat, align 8
  %tmp___4 = alloca i32*, align 8
  %tmp___5 = alloca i32, align 4
  %tmp___6 = alloca i32*, align 8
  store %struct.pipe* %the_pipe, %struct.pipe** %1, align 8
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  store i8* null, i8** %file, align 8
  store i32 0, i32* %arg_error, align 4
  %4 = load i32* @optind, align 4
  store i32 %4, i32* %arg_index, align 4
  %5 = load %struct.pipe** %1, align 8
  %6 = getelementptr inbounds %struct.pipe* %5, i32 0, i32 3
  %7 = load i32* %6, align 4
  %8 = call i32 @close(i32 %7)
  %9 = load %struct.pipe** %1, align 8
  %10 = getelementptr inbounds %struct.pipe* %9, i32 0, i32 4
  %11 = load i32* %10, align 4
  %12 = call i32 @close(i32 %11)
  %13 = load %struct.pipe** %1, align 8
  %14 = getelementptr inbounds %struct.pipe* %13, i32 0, i32 5
  %15 = load i32* %14, align 4
  %16 = call i32 @close(i32 %15)
  %17 = load %struct.pipe** %1, align 8
  call void @read_ispell_errors(%struct.pipe* %17)
  store i32 0, i32* %pos, align 4
  %18 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %18, %struct.str** %tmp, align 8
  %19 = load %struct.str** %tmp, align 8
  store %struct.str* %19, %struct.str** %ispell_version, align 8
  %20 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %20, %struct.str** %tmp___0, align 8
  %21 = load %struct.str** %tmp___0, align 8
  store %struct.str* %21, %struct.str** %str, align 8
  %22 = load %struct.str** %str, align 8
  %23 = load %struct.pipe** %1, align 8
  %24 = getelementptr inbounds %struct.pipe* %23, i32 0, i32 0
  %25 = load i32* %24, align 4
  %26 = call i32 @str_add_line_from_desc(%struct.str* %22, i32 %25)
  store i32 %26, i32* %tmp___1, align 4
  %27 = load i32* %tmp___1, align 4
  %28 = icmp eq i32 %27, 2
  br i1 %28, label %29, label %30

; <label>:29                                      ; preds = %0
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([35 x i8]* @.str34, i32 0, i32 0))
  br label %30

; <label>:30                                      ; preds = %29, %0
  br label %31

; <label>:31                                      ; preds = %30, %59
  br label %32

; <label>:32                                      ; preds = %31
  %33 = call i16** @__ctype_b_loc() #7
  store i16** %33, i16*** %tmp___2, align 8
  %34 = load i16*** %tmp___2, align 8
  %35 = load i16** %34, align 8
  %36 = load %struct.str** %str, align 8
  %37 = getelementptr inbounds %struct.str* %36, i32 0, i32 0
  %38 = load i8** %37, align 8
  %39 = load i32* %pos, align 4
  %40 = sext i32 %39 to i64
  %41 = getelementptr inbounds i8* %38, i64 %40
  %42 = load i8* %41, align 1
  %43 = sext i8 %42 to i32
  %44 = sext i32 %43 to i64
  %45 = getelementptr inbounds i16* %35, i64 %44
  %46 = load i16* %45, align 2
  %47 = zext i16 %46 to i32
  %48 = and i32 %47, 2048
  %49 = icmp ne i32 %48, 0
  br i1 %49, label %50, label %51

; <label>:50                                      ; preds = %32
  br label %62

; <label>:51                                      ; preds = %32
  %52 = load i32* %pos, align 4
  %53 = load %struct.str** %str, align 8
  %54 = getelementptr inbounds %struct.str* %53, i32 0, i32 1
  %55 = load i32* %54, align 4
  %56 = icmp sle i32 %52, %55
  br i1 %56, label %58, label %57

; <label>:57                                      ; preds = %51
  br label %62

; <label>:58                                      ; preds = %51
  br label %59

; <label>:59                                      ; preds = %58
  %60 = load i32* %pos, align 4
  %61 = add nsw i32 %60, 1
  store i32 %61, i32* %pos, align 4
  br label %31

; <label>:62                                      ; preds = %57, %50
  br label %63

; <label>:63                                      ; preds = %62, %83
  br label %64

; <label>:64                                      ; preds = %63
  %65 = load %struct.str** %str, align 8
  %66 = getelementptr inbounds %struct.str* %65, i32 0, i32 0
  %67 = load i8** %66, align 8
  %68 = load i32* %pos, align 4
  %69 = sext i32 %68 to i64
  %70 = getelementptr inbounds i8* %67, i64 %69
  %71 = load i8* %70, align 1
  %72 = sext i8 %71 to i32
  %73 = icmp ne i32 %72, 32
  br i1 %73, label %74, label %82

; <label>:74                                      ; preds = %64
  %75 = load i32* %pos, align 4
  %76 = load %struct.str** %str, align 8
  %77 = getelementptr inbounds %struct.str* %76, i32 0, i32 1
  %78 = load i32* %77, align 4
  %79 = icmp sle i32 %75, %78
  br i1 %79, label %81, label %80

; <label>:80                                      ; preds = %74
  br label %94

; <label>:81                                      ; preds = %74
  br label %83

; <label>:82                                      ; preds = %64
  br label %94

; <label>:83                                      ; preds = %81
  %84 = load %struct.str** %ispell_version, align 8
  %85 = load %struct.str** %str, align 8
  %86 = getelementptr inbounds %struct.str* %85, i32 0, i32 0
  %87 = load i8** %86, align 8
  %88 = load i32* %pos, align 4
  %89 = sext i32 %88 to i64
  %90 = getelementptr inbounds i8* %87, i64 %89
  %91 = load i8* %90, align 1
  call void @str_add_char(%struct.str* %84, i8 signext %91)
  %92 = load i32* %pos, align 4
  %93 = add nsw i32 %92, 1
  store i32 %93, i32* %pos, align 4
  br label %63

; <label>:94                                      ; preds = %82, %80
  %95 = load i32* @show_ispell_version, align 4
  %96 = icmp ne i32 %95, 0
  br i1 %96, label %97, label %103

; <label>:97                                      ; preds = %94
  %98 = load %struct.str** %ispell_version, align 8
  %99 = call i8* @str_to_nstr(%struct.str* %98)
  store i8* %99, i8** %tmp___3, align 8
  %100 = load i8** @program_name, align 8
  %101 = load i8** %tmp___3, align 8
  %102 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([23 x i8]* @.str35, i32 0, i32 0), i8* %100, i8* %101)
  call void @exit(i32 0) #8
  unreachable

; <label>:103                                     ; preds = %94
  %104 = call i8* @xstrdup(i8* getelementptr inbounds ([2 x i8]* @.str36, i32 0, i32 0))
  store i8* %104, i8** %file, align 8
  %105 = load i32* %2, align 4
  %106 = icmp eq i32 %105, 1
  br i1 %106, label %107, label %110

; <label>:107                                     ; preds = %103
  %108 = load %struct.pipe** %1, align 8
  %109 = load %struct._IO_FILE** @stdin, align 8
  call void @read_file(%struct.pipe* %108, %struct._IO_FILE* %109, i8* getelementptr inbounds ([2 x i8]* @.str36, i32 0, i32 0))
  br label %110

; <label>:110                                     ; preds = %107, %103
  br label %111

; <label>:111                                     ; preds = %110, %182
  br label %112

; <label>:112                                     ; preds = %160, %148, %111
  %113 = load i32* %arg_index, align 4
  %114 = load i32* %2, align 4
  %115 = icmp slt i32 %113, %114
  br i1 %115, label %117, label %116

; <label>:116                                     ; preds = %112
  br label %185

; <label>:117                                     ; preds = %112
  store i32 0, i32* %arg_error, align 4
  %118 = load i8*** %3, align 8
  %119 = load i32* %arg_index, align 4
  %120 = sext i32 %119 to i64
  %121 = getelementptr inbounds i8** %118, i64 %120
  %122 = load i8** %121, align 8
  store i8* %122, i8** %file, align 8
  %123 = load i8** %file, align 8
  %124 = getelementptr inbounds i8* %123, i64 0
  %125 = load i8* %124, align 1
  %126 = sext i8 %125 to i32
  %127 = icmp eq i32 %126, 45
  br i1 %127, label %128, label %142

; <label>:128                                     ; preds = %117
  %129 = load i8** %file, align 8
  %130 = getelementptr inbounds i8* %129, i64 1
  %131 = load i8* %130, align 1
  %132 = sext i8 %131 to i32
  %133 = icmp eq i32 %132, 0
  br i1 %133, label %134, label %140

; <label>:134                                     ; preds = %128
  %135 = load i32* @read_stdin, align 4
  %136 = icmp ne i32 %135, 0
  br i1 %136, label %139, label %137

; <label>:137                                     ; preds = %134
  store i32 1, i32* @read_stdin, align 4
  %138 = load %struct._IO_FILE** @stdin, align 8
  store %struct._IO_FILE* %138, %struct._IO_FILE** %stream, align 8
  br label %139

; <label>:139                                     ; preds = %137, %134
  br label %141

; <label>:140                                     ; preds = %128
  br label %143

; <label>:141                                     ; preds = %139
  br label %175

; <label>:142                                     ; preds = %117
  br label %143

; <label>:143                                     ; preds = %142, %140
  %144 = load i8** %file, align 8
  %145 = call i32 @stat(i8* %144, %struct.stat* %stat_buf)
  store i32 %145, i32* %tmp___5, align 4
  %146 = load i32* %tmp___5, align 4
  %147 = icmp eq i32 %146, -1
  br i1 %147, label %148, label %155

; <label>:148                                     ; preds = %143
  %149 = call i32* @__errno_location() #7
  store i32* %149, i32** %tmp___4, align 8
  %150 = load i32** %tmp___4, align 8
  %151 = load i32* %150, align 4
  %152 = load i8** %file, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 %151, i8* getelementptr inbounds ([15 x i8]* @.str37, i32 0, i32 0), i8* %152)
  %153 = load i32* %arg_index, align 4
  %154 = add nsw i32 %153, 1
  store i32 %154, i32* %arg_index, align 4
  br label %112

; <label>:155                                     ; preds = %143
  %156 = getelementptr inbounds %struct.stat* %stat_buf, i32 0, i32 3
  %157 = load i32* %156, align 4
  %158 = and i32 %157, 61440
  %159 = icmp eq i32 %158, 16384
  br i1 %159, label %160, label %164

; <label>:160                                     ; preds = %155
  %161 = load i8** %file, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 0, i8* getelementptr inbounds ([19 x i8]* @.str38, i32 0, i32 0), i8* %161)
  %162 = load i32* %arg_index, align 4
  %163 = add nsw i32 %162, 1
  store i32 %163, i32* %arg_index, align 4
  br label %112

; <label>:164                                     ; preds = %155
  %165 = load i8** %file, align 8
  %166 = call %struct._IO_FILE* @fopen(i8* %165, i8* getelementptr inbounds ([2 x i8]* @.str39, i32 0, i32 0))
  store %struct._IO_FILE* %166, %struct._IO_FILE** %stream, align 8
  %167 = load %struct._IO_FILE** %stream, align 8
  %168 = icmp ne %struct._IO_FILE* %167, null
  br i1 %168, label %174, label %169

; <label>:169                                     ; preds = %164
  %170 = call i32* @__errno_location() #7
  store i32* %170, i32** %tmp___6, align 8
  %171 = load i32** %tmp___6, align 8
  %172 = load i32* %171, align 4
  %173 = load i8** %file, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 %172, i8* getelementptr inbounds ([15 x i8]* @.str40, i32 0, i32 0), i8* %173)
  store i32 1, i32* %arg_error, align 4
  br label %174

; <label>:174                                     ; preds = %169, %164
  br label %175

; <label>:175                                     ; preds = %174, %141
  %176 = load i32* %arg_error, align 4
  %177 = icmp ne i32 %176, 0
  br i1 %177, label %182, label %178

; <label>:178                                     ; preds = %175
  %179 = load %struct.pipe** %1, align 8
  %180 = load %struct._IO_FILE** %stream, align 8
  %181 = load i8** %file, align 8
  call void @read_file(%struct.pipe* %179, %struct._IO_FILE* %180, i8* %181)
  br label %182

; <label>:182                                     ; preds = %178, %175
  %183 = load i32* %arg_index, align 4
  %184 = add nsw i32 %183, 1
  store i32 %184, i32* %arg_index, align 4
  br label %111

; <label>:185                                     ; preds = %116
  ret void
}

; Function Attrs: nounwind uwtable
define void @run_ispell_in_child(%struct.pipe* %the_pipe) #0 {
  %1 = alloca %struct.pipe*, align 8
  %tmp = alloca i32*, align 8
  %tmp___0 = alloca i32, align 4
  %tmp___1 = alloca i32*, align 8
  %tmp___2 = alloca i32, align 4
  %tmp___3 = alloca i32*, align 8
  %tmp___4 = alloca i32, align 4
  %tmp___5 = alloca i32*, align 8
  %tmp___6 = alloca i32, align 4
  %tmp___7 = alloca i32*, align 8
  %tmp___8 = alloca i32, align 4
  %tmp___9 = alloca i32*, align 8
  %tmp___10 = alloca i32, align 4
  store %struct.pipe* %the_pipe, %struct.pipe** %1, align 8
  %2 = load %struct.pipe** %1, align 8
  %3 = getelementptr inbounds %struct.pipe* %2, i32 0, i32 0
  %4 = load i32* %3, align 4
  %5 = call i32 @close(i32 %4)
  %6 = load %struct.pipe** %1, align 8
  %7 = getelementptr inbounds %struct.pipe* %6, i32 0, i32 1
  %8 = load i32* %7, align 4
  %9 = call i32 @close(i32 %8)
  %10 = load %struct.pipe** %1, align 8
  %11 = getelementptr inbounds %struct.pipe* %10, i32 0, i32 2
  %12 = load i32* %11, align 4
  %13 = call i32 @close(i32 %12)
  %14 = load %struct.pipe** %1, align 8
  %15 = getelementptr inbounds %struct.pipe* %14, i32 0, i32 3
  %16 = load i32* %15, align 4
  %17 = icmp ne i32 %16, 0
  br i1 %17, label %18, label %30

; <label>:18                                      ; preds = %0
  %19 = load %struct.pipe** %1, align 8
  %20 = getelementptr inbounds %struct.pipe* %19, i32 0, i32 3
  %21 = load i32* %20, align 4
  %22 = call i32 @dup2(i32 %21, i32 0)
  store i32 %22, i32* %tmp___0, align 4
  %23 = load i32* %tmp___0, align 4
  %24 = icmp ne i32 %23, 0
  br i1 %24, label %25, label %29

; <label>:25                                      ; preds = %18
  %26 = call i32* @__errno_location() #7
  store i32* %26, i32** %tmp, align 8
  %27 = load i32** %tmp, align 8
  %28 = load i32* %27, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %28, i8* getelementptr inbounds ([22 x i8]* @.str41, i32 0, i32 0))
  br label %29

; <label>:29                                      ; preds = %25, %18
  br label %30

; <label>:30                                      ; preds = %29, %0
  %31 = load %struct.pipe** %1, align 8
  %32 = getelementptr inbounds %struct.pipe* %31, i32 0, i32 4
  %33 = load i32* %32, align 4
  %34 = icmp ne i32 %33, 1
  br i1 %34, label %35, label %47

; <label>:35                                      ; preds = %30
  %36 = load %struct.pipe** %1, align 8
  %37 = getelementptr inbounds %struct.pipe* %36, i32 0, i32 4
  %38 = load i32* %37, align 4
  %39 = call i32 @dup2(i32 %38, i32 1)
  store i32 %39, i32* %tmp___2, align 4
  %40 = load i32* %tmp___2, align 4
  %41 = icmp ne i32 %40, 1
  br i1 %41, label %42, label %46

; <label>:42                                      ; preds = %35
  %43 = call i32* @__errno_location() #7
  store i32* %43, i32** %tmp___1, align 8
  %44 = load i32** %tmp___1, align 8
  %45 = load i32* %44, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %45, i8* getelementptr inbounds ([23 x i8]* @.str42, i32 0, i32 0))
  br label %46

; <label>:46                                      ; preds = %42, %35
  br label %47

; <label>:47                                      ; preds = %46, %30
  %48 = load %struct.pipe** %1, align 8
  %49 = getelementptr inbounds %struct.pipe* %48, i32 0, i32 5
  %50 = load i32* %49, align 4
  %51 = icmp ne i32 %50, 2
  br i1 %51, label %52, label %64

; <label>:52                                      ; preds = %47
  %53 = load %struct.pipe** %1, align 8
  %54 = getelementptr inbounds %struct.pipe* %53, i32 0, i32 5
  %55 = load i32* %54, align 4
  %56 = call i32 @dup2(i32 %55, i32 2)
  store i32 %56, i32* %tmp___4, align 4
  %57 = load i32* %tmp___4, align 4
  %58 = icmp ne i32 %57, 2
  br i1 %58, label %59, label %63

; <label>:59                                      ; preds = %52
  %60 = call i32* @__errno_location() #7
  store i32* %60, i32** %tmp___3, align 8
  %61 = load i32** %tmp___3, align 8
  %62 = load i32* %61, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %62, i8* getelementptr inbounds ([23 x i8]* @.str43, i32 0, i32 0))
  br label %63

; <label>:63                                      ; preds = %59, %52
  br label %64

; <label>:64                                      ; preds = %63, %47
  %65 = load i8** @dictionary, align 8
  %66 = ptrtoint i8* %65 to i64
  %67 = icmp ne i64 %66, 0
  br i1 %67, label %68, label %80

; <label>:68                                      ; preds = %64
  %69 = load i8** @ispell_prog, align 8
  %70 = load i8** @dictionary, align 8
  %71 = call i32 (i8*, i8*, ...)* @execl(i8* %69, i8* getelementptr inbounds ([7 x i8]* @.str5, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @.str44, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @.str45, i32 0, i32 0), i8* %70, i8* null)
  store i32 %71, i32* %tmp___6, align 4
  %72 = load i32* %tmp___6, align 4
  %73 = icmp slt i32 %72, 0
  br i1 %73, label %74, label %79

; <label>:74                                      ; preds = %68
  %75 = call i32* @__errno_location() #7
  store i32* %75, i32** %tmp___5, align 8
  %76 = load i32** %tmp___5, align 8
  %77 = load i32* %76, align 4
  %78 = load i8** @ispell_prog, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %77, i8* getelementptr inbounds ([19 x i8]* @.str46, i32 0, i32 0), i8* %78)
  br label %79

; <label>:79                                      ; preds = %74, %68
  br label %80

; <label>:80                                      ; preds = %79, %64
  %81 = load i32* @british, align 4
  %82 = icmp ne i32 %81, 0
  br i1 %82, label %83, label %94

; <label>:83                                      ; preds = %80
  %84 = load i8** @ispell_prog, align 8
  %85 = call i32 (i8*, i8*, ...)* @execl(i8* %84, i8* getelementptr inbounds ([7 x i8]* @.str5, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @.str44, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @.str47, i32 0, i32 0), i8* getelementptr inbounds ([8 x i8]* @.str2, i32 0, i32 0), i8* null)
  store i32 %85, i32* %tmp___8, align 4
  %86 = load i32* %tmp___8, align 4
  %87 = icmp slt i32 %86, 0
  br i1 %87, label %88, label %93

; <label>:88                                      ; preds = %83
  %89 = call i32* @__errno_location() #7
  store i32* %89, i32** %tmp___7, align 8
  %90 = load i32** %tmp___7, align 8
  %91 = load i32* %90, align 4
  %92 = load i8** @ispell_prog, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %91, i8* getelementptr inbounds ([19 x i8]* @.str46, i32 0, i32 0), i8* %92)
  br label %93

; <label>:93                                      ; preds = %88, %83
  br label %94

; <label>:94                                      ; preds = %93, %80
  %95 = load i8** @ispell_prog, align 8
  %96 = call i32 (i8*, i8*, ...)* @execl(i8* %95, i8* getelementptr inbounds ([7 x i8]* @.str5, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @.str44, i32 0, i32 0), i8* null)
  store i32 %96, i32* %tmp___10, align 4
  %97 = load i32* %tmp___10, align 4
  %98 = icmp slt i32 %97, 0
  br i1 %98, label %99, label %104

; <label>:99                                      ; preds = %94
  %100 = call i32* @__errno_location() #7
  store i32* %100, i32** %tmp___9, align 8
  %101 = load i32** %tmp___9, align 8
  %102 = load i32* %101, align 4
  %103 = load i8** @ispell_prog, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %102, i8* getelementptr inbounds ([19 x i8]* @.str46, i32 0, i32 0), i8* %103)
  br label %104

; <label>:104                                     ; preds = %99, %94
  ret void
}

declare i8* @getenv(i8*) #1

; Function Attrs: nounwind readonly
declare i32 @strlen(i8*) #5

; Function Attrs: noreturn
declare void @abort() #3

; Function Attrs: nounwind uwtable
define void @read_file(%struct.pipe* %the_pipe, %struct._IO_FILE* %stream, i8* %file) #0 {
  %1 = alloca %struct.pipe*, align 8
  %2 = alloca %struct._IO_FILE*, align 8
  %3 = alloca i8*, align 8
  %str = alloca %struct.str*, align 8
  %tmp = alloca %struct.str*, align 8
  %add_line_ret = alloca i32, align 4
  %line = alloca i32, align 4
  %tmp___0 = alloca i32, align 4
  %tmp___1 = alloca i32*, align 8
  %tmp___2 = alloca i32*, align 8
  %tmp___3 = alloca i8*, align 8
  %tmp___4 = alloca i32, align 4
  %tmp___5 = alloca i32*, align 8
  %tmp___6 = alloca i32, align 4
  store %struct.pipe* %the_pipe, %struct.pipe** %1, align 8
  store %struct._IO_FILE* %stream, %struct._IO_FILE** %2, align 8
  store i8* %file, i8** %3, align 8
  %4 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %4, %struct.str** %tmp, align 8
  %5 = load %struct.str** %tmp, align 8
  store %struct.str* %5, %struct.str** %str, align 8
  store i32 0, i32* %add_line_ret, align 4
  store i32 0, i32* %line, align 4
  br label %6

; <label>:6                                       ; preds = %0, %78
  br label %7

; <label>:7                                       ; preds = %6
  %8 = load %struct.str** %str, align 8
  %9 = call %struct.str* @str_make(%struct.str* %8)
  store %struct.str* %9, %struct.str** %str, align 8
  %10 = load %struct.str** %str, align 8
  call void @str_add_char(%struct.str* %10, i8 signext 94)
  %11 = load %struct.str** %str, align 8
  %12 = load %struct._IO_FILE** %2, align 8
  %13 = call i32 @str_add_line(%struct.str* %11, %struct._IO_FILE* %12)
  store i32 %13, i32* %tmp___0, align 4
  %14 = load i32* %tmp___0, align 4
  store i32 %14, i32* %add_line_ret, align 4
  %15 = load i32* %line, align 4
  %16 = add nsw i32 %15, 1
  store i32 %16, i32* %line, align 4
  %17 = load i32* %add_line_ret, align 4
  %18 = icmp eq i32 %17, 1
  br i1 %18, label %19, label %24

; <label>:19                                      ; preds = %7
  %20 = call i32* @__errno_location() #7
  store i32* %20, i32** %tmp___1, align 8
  %21 = load i32** %tmp___1, align 8
  %22 = load i32* %21, align 4
  %23 = load i8** %3, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %22, i8* getelementptr inbounds ([23 x i8]* @.str21, i32 0, i32 0), i8* %23)
  br label %24

; <label>:24                                      ; preds = %19, %7
  %25 = load i32* %add_line_ret, align 4
  %26 = icmp eq i32 %25, 2
  br i1 %26, label %27, label %34

; <label>:27                                      ; preds = %24
  %28 = load %struct.str** %str, align 8
  %29 = getelementptr inbounds %struct.str* %28, i32 0, i32 1
  %30 = load i32* %29, align 4
  %31 = icmp ne i32 %30, 0
  br i1 %31, label %33, label %32

; <label>:32                                      ; preds = %27
  br label %89

; <label>:33                                      ; preds = %27
  br label %34

; <label>:34                                      ; preds = %33, %24
  %35 = load %struct.str** %str, align 8
  %36 = getelementptr inbounds %struct.str* %35, i32 0, i32 0
  %37 = load i8** %36, align 8
  %38 = load %struct.str** %str, align 8
  %39 = getelementptr inbounds %struct.str* %38, i32 0, i32 1
  %40 = load i32* %39, align 4
  %41 = sub nsw i32 %40, 1
  %42 = sext i32 %41 to i64
  %43 = getelementptr inbounds i8* %37, i64 %42
  %44 = load i8* %43, align 1
  %45 = sext i8 %44 to i32
  %46 = icmp ne i32 %45, 10
  br i1 %46, label %47, label %49

; <label>:47                                      ; preds = %34
  %48 = load %struct.str** %str, align 8
  call void @str_add_char(%struct.str* %48, i8 signext 10)
  br label %49

; <label>:49                                      ; preds = %47, %34
  %50 = load %struct.str** %str, align 8
  %51 = call i8* @str_to_nstr(%struct.str* %50)
  store i8* %51, i8** %tmp___3, align 8
  %52 = load %struct.pipe** %1, align 8
  %53 = getelementptr inbounds %struct.pipe* %52, i32 0, i32 1
  %54 = load i32* %53, align 4
  %55 = load i8** %tmp___3, align 8
  %56 = load %struct.str** %str, align 8
  %57 = getelementptr inbounds %struct.str* %56, i32 0, i32 1
  %58 = load i32* %57, align 4
  %59 = call i32 @write(i32 %54, i8* %55, i32 %58)
  store i32 %59, i32* %tmp___4, align 4
  %60 = load i32* %tmp___4, align 4
  %61 = load %struct.str** %str, align 8
  %62 = getelementptr inbounds %struct.str* %61, i32 0, i32 1
  %63 = load i32* %62, align 4
  %64 = icmp ne i32 %60, %63
  br i1 %64, label %65, label %69

; <label>:65                                      ; preds = %49
  %66 = call i32* @__errno_location() #7
  store i32* %66, i32** %tmp___2, align 8
  %67 = load i32** %tmp___2, align 8
  %68 = load i32* %67, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 %68, i8* getelementptr inbounds ([24 x i8]* @.str22, i32 0, i32 0))
  br label %69

; <label>:69                                      ; preds = %65, %49
  %70 = load %struct.pipe** %1, align 8
  call void @read_ispell_errors(%struct.pipe* %70)
  %71 = load %struct.pipe** %1, align 8
  %72 = load i8** %3, align 8
  %73 = load i32* %line, align 4
  call void @read_ispell(%struct.pipe* %71, i8* %72, i32 %73)
  %74 = load %struct.pipe** %1, align 8
  call void @read_ispell_errors(%struct.pipe* %74)
  %75 = load i32* %add_line_ret, align 4
  %76 = icmp eq i32 %75, 2
  br i1 %76, label %77, label %78

; <label>:77                                      ; preds = %69
  br label %89

; <label>:78                                      ; preds = %69
  br label %6
                                                  ; No predecessors!
  %80 = load %struct._IO_FILE** %2, align 8
  %81 = call i32 @fclose(%struct._IO_FILE* %80)
  store i32 %81, i32* %tmp___6, align 4
  %82 = load i32* %tmp___6, align 4
  %83 = icmp eq i32 %82, -1
  br i1 %83, label %84, label %89

; <label>:84                                      ; preds = %79
  %85 = call i32* @__errno_location() #7
  store i32* %85, i32** %tmp___5, align 8
  %86 = load i32** %tmp___5, align 8
  %87 = load i32* %86, align 4
  %88 = load i8** %3, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 %87, i8* getelementptr inbounds ([16 x i8]* @.str23, i32 0, i32 0), i8* %88)
  br label %89

; <label>:89                                      ; preds = %32, %77, %84, %79
  ret void
}

declare i32 @write(i32, i8*, i32) #1

; Function Attrs: nounwind uwtable
define void @read_ispell_errors(%struct.pipe* %the_pipe) #0 {
  %1 = alloca %struct.pipe*, align 8
  %time_out = alloca %struct.timeval, align 8
  %str = alloca %struct.str*, align 8
  %tmp = alloca %struct.str*, align 8
  %tmp___0 = alloca i32, align 4
  %tmp___1 = alloca i32, align 4
  %tmp___2 = alloca i32, align 4
  %tmp___3 = alloca i32, align 4
  %tmp___4 = alloca i32, align 4
  store %struct.pipe* %the_pipe, %struct.pipe** %1, align 8
  %2 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %2, %struct.str** %tmp, align 8
  %3 = load %struct.str** %tmp, align 8
  store %struct.str* %3, %struct.str** %str, align 8
  %4 = getelementptr inbounds %struct.timeval* %time_out, i32 0, i32 1
  store i64 0, i64* %4, align 8
  %5 = getelementptr inbounds %struct.timeval* %time_out, i32 0, i32 1
  %6 = load i64* %5, align 8
  %7 = getelementptr inbounds %struct.timeval* %time_out, i32 0, i32 0
  store i64 %6, i64* %7, align 8
  br label %8

; <label>:8                                       ; preds = %0, %57
  br label %9

; <label>:9                                       ; preds = %8
  %10 = load %struct.pipe** %1, align 8
  %11 = getelementptr inbounds %struct.pipe* %10, i32 0, i32 6
  %12 = call i32 @select(i32 1024, %struct.__anonstruct_fd_set_13* %11, %struct.__anonstruct_fd_set_13* null, %struct.__anonstruct_fd_set_13* null, %struct.timeval* %time_out)
  store i32 %12, i32* %tmp___4, align 4
  %13 = load i32* %tmp___4, align 4
  %14 = icmp eq i32 %13, 1
  br i1 %14, label %16, label %15

; <label>:15                                      ; preds = %9
  br label %64

; <label>:16                                      ; preds = %9
  %17 = load %struct.str** %str, align 8
  %18 = call %struct.str* @str_make(%struct.str* %17)
  store %struct.str* %18, %struct.str** %str, align 8
  %19 = load %struct.str** %str, align 8
  %20 = load %struct.pipe** %1, align 8
  %21 = getelementptr inbounds %struct.pipe* %20, i32 0, i32 2
  %22 = load i32* %21, align 4
  %23 = call i32 @str_add_line_from_desc(%struct.str* %19, i32 %22)
  store i32 %23, i32* %tmp___0, align 4
  %24 = load i32* %tmp___0, align 4
  %25 = icmp eq i32 %24, 2
  br i1 %25, label %26, label %27

; <label>:26                                      ; preds = %16
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([35 x i8]* @.str27, i32 0, i32 0))
  br label %27

; <label>:27                                      ; preds = %26, %16
  %28 = load %struct.str** %str, align 8
  %29 = getelementptr inbounds %struct.str* %28, i32 0, i32 1
  %30 = load i32* %29, align 4
  %31 = sub nsw i32 %30, 2
  store i32 %31, i32* %29, align 4
  %32 = load %struct.str** %str, align 8
  %33 = getelementptr inbounds %struct.str* %32, i32 0, i32 0
  %34 = load i8** %33, align 8
  %35 = load %struct.str** %str, align 8
  %36 = getelementptr inbounds %struct.str* %35, i32 0, i32 1
  %37 = load i32* %36, align 4
  %38 = sub nsw i32 %37, 1
  %39 = sext i32 %38 to i64
  %40 = getelementptr inbounds i8* %34, i64 %39
  store i8 0, i8* %40, align 1
  %41 = call i32 @strlen(i8* getelementptr inbounds ([12 x i8]* @.str28, i32 0, i32 0)) #9
  store i32 %41, i32* %tmp___2, align 4
  %42 = load %struct.str** %str, align 8
  %43 = getelementptr inbounds %struct.str* %42, i32 0, i32 0
  %44 = load i8** %43, align 8
  %45 = load i32* %tmp___2, align 4
  %46 = call i32 @memcmp(i8* %44, i8* getelementptr inbounds ([12 x i8]* @.str28, i32 0, i32 0), i32 %45) #9
  store i32 %46, i32* %tmp___3, align 4
  %47 = load i32* %tmp___3, align 4
  %48 = icmp ne i32 %47, 0
  br i1 %48, label %57, label %49

; <label>:49                                      ; preds = %27
  %50 = call i32 @strlen(i8* getelementptr inbounds ([12 x i8]* @.str28, i32 0, i32 0)) #9
  store i32 %50, i32* %tmp___1, align 4
  %51 = load %struct.str** %str, align 8
  %52 = getelementptr inbounds %struct.str* %51, i32 0, i32 0
  %53 = load i8** %52, align 8
  %54 = load i32* %tmp___1, align 4
  %55 = zext i32 %54 to i64
  %56 = getelementptr inbounds i8* %53, i64 %55
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([16 x i8]* @.str29, i32 0, i32 0), i8* %56)
  br label %57

; <label>:57                                      ; preds = %49, %27
  %58 = load %struct._IO_FILE** @stderr, align 8
  %59 = load i8** @ispell_prog, align 8
  %60 = load %struct.str** %str, align 8
  %61 = getelementptr inbounds %struct.str* %60, i32 0, i32 0
  %62 = load i8** %61, align 8
  %63 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %58, i8* getelementptr inbounds ([8 x i8]* @.str30, i32 0, i32 0), i8* %59, i8* %62)
  br label %8

; <label>:64                                      ; preds = %15
  ret void
}

; Function Attrs: nounwind uwtable
define void @read_ispell(%struct.pipe* %ispell_pipe, i8* %file, i32 %line) #0 {
  %1 = alloca %struct.pipe*, align 8
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  %str = alloca %struct.str*, align 8
  %tmp = alloca %struct.str*, align 8
  %tmp___0 = alloca i32, align 4
  %pos = alloca i32, align 4
  %tmp___1 = alloca i8*, align 8
  store %struct.pipe* %ispell_pipe, %struct.pipe** %1, align 8
  store i8* %file, i8** %2, align 8
  store i32 %line, i32* %3, align 4
  %4 = call %struct.str* @str_make(%struct.str* null)
  store %struct.str* %4, %struct.str** %tmp, align 8
  %5 = load %struct.str** %tmp, align 8
  store %struct.str* %5, %struct.str** %str, align 8
  br label %6

; <label>:6                                       ; preds = %0, %152
  br label %7

; <label>:7                                       ; preds = %150, %134, %59, %50, %41, %6
  %8 = load %struct.str** %str, align 8
  %9 = call %struct.str* @str_make(%struct.str* %8)
  store %struct.str* %9, %struct.str** %str, align 8
  %10 = load %struct.str** %str, align 8
  %11 = load %struct.pipe** %1, align 8
  %12 = getelementptr inbounds %struct.pipe* %11, i32 0, i32 0
  %13 = load i32* %12, align 4
  %14 = call i32 @str_add_line_from_desc(%struct.str* %10, i32 %13)
  store i32 %14, i32* %tmp___0, align 4
  %15 = load i32* %tmp___0, align 4
  %16 = icmp eq i32 %15, 2
  br i1 %16, label %17, label %18

; <label>:17                                      ; preds = %7
  call void @exit(i32 0) #8
  unreachable

; <label>:18                                      ; preds = %7
  %19 = load %struct.str** %str, align 8
  %20 = getelementptr inbounds %struct.str* %19, i32 0, i32 1
  %21 = load i32* %20, align 4
  %22 = icmp eq i32 %21, 1
  br i1 %22, label %23, label %33

; <label>:23                                      ; preds = %18
  %24 = load %struct.str** %str, align 8
  %25 = getelementptr inbounds %struct.str* %24, i32 0, i32 0
  %26 = load i8** %25, align 8
  %27 = getelementptr inbounds i8* %26, i64 0
  %28 = load i8* %27, align 1
  %29 = sext i8 %28 to i32
  %30 = icmp eq i32 %29, 10
  br i1 %30, label %31, label %32

; <label>:31                                      ; preds = %23
  br label %156

; <label>:32                                      ; preds = %23
  br label %33

; <label>:33                                      ; preds = %32, %18
  %34 = load %struct.str** %str, align 8
  %35 = getelementptr inbounds %struct.str* %34, i32 0, i32 0
  %36 = load i8** %35, align 8
  %37 = getelementptr inbounds i8* %36, i64 0
  %38 = load i8* %37, align 1
  %39 = sext i8 %38 to i32
  %40 = icmp eq i32 %39, 42
  br i1 %40, label %41, label %42

; <label>:41                                      ; preds = %33
  br label %7

; <label>:42                                      ; preds = %33
  %43 = load %struct.str** %str, align 8
  %44 = getelementptr inbounds %struct.str* %43, i32 0, i32 0
  %45 = load i8** %44, align 8
  %46 = getelementptr inbounds i8* %45, i64 0
  %47 = load i8* %46, align 1
  %48 = sext i8 %47 to i32
  %49 = icmp eq i32 %48, 43
  br i1 %49, label %50, label %51

; <label>:50                                      ; preds = %42
  br label %7

; <label>:51                                      ; preds = %42
  %52 = load %struct.str** %str, align 8
  %53 = getelementptr inbounds %struct.str* %52, i32 0, i32 0
  %54 = load i8** %53, align 8
  %55 = getelementptr inbounds i8* %54, i64 0
  %56 = load i8* %55, align 1
  %57 = sext i8 %56 to i32
  %58 = icmp eq i32 %57, 45
  br i1 %58, label %59, label %60

; <label>:59                                      ; preds = %51
  br label %7

; <label>:60                                      ; preds = %51
  br label %61

; <label>:61                                      ; preds = %60
  br label %62

; <label>:62                                      ; preds = %61
  %63 = load %struct.str** %str, align 8
  %64 = getelementptr inbounds %struct.str* %63, i32 0, i32 0
  %65 = load i8** %64, align 8
  %66 = getelementptr inbounds i8* %65, i64 0
  %67 = load i8* %66, align 1
  %68 = sext i8 %67 to i32
  %69 = icmp eq i32 %68, 38
  br i1 %69, label %70, label %71

; <label>:70                                      ; preds = %62
  br label %92

; <label>:71                                      ; preds = %62
  %72 = load %struct.str** %str, align 8
  %73 = getelementptr inbounds %struct.str* %72, i32 0, i32 0
  %74 = load i8** %73, align 8
  %75 = getelementptr inbounds i8* %74, i64 0
  %76 = load i8* %75, align 1
  %77 = sext i8 %76 to i32
  %78 = icmp eq i32 %77, 35
  br i1 %78, label %79, label %80

; <label>:79                                      ; preds = %71
  br label %92

; <label>:80                                      ; preds = %71
  %81 = load %struct.str** %str, align 8
  %82 = getelementptr inbounds %struct.str* %81, i32 0, i32 0
  %83 = load i8** %82, align 8
  %84 = getelementptr inbounds i8* %83, i64 0
  %85 = load i8* %84, align 1
  %86 = sext i8 %85 to i32
  %87 = icmp eq i32 %86, 63
  br i1 %87, label %88, label %137

; <label>:88                                      ; preds = %80
  %89 = load i32* @verbose, align 4
  %90 = icmp ne i32 %89, 0
  br i1 %90, label %91, label %136

; <label>:91                                      ; preds = %88
  br label %92

; <label>:92                                      ; preds = %91, %79, %70
  %93 = load i32* @print_file_names, align 4
  %94 = icmp ne i32 %93, 0
  br i1 %94, label %95, label %103

; <label>:95                                      ; preds = %92
  %96 = load i8** %2, align 8
  %97 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str24, i32 0, i32 0), i8* %96)
  %98 = load i32* @number_lines, align 4
  %99 = icmp ne i32 %98, 0
  br i1 %99, label %102, label %100

; <label>:100                                     ; preds = %95
  %101 = call i32 @putchar(i32 32)
  br label %102

; <label>:102                                     ; preds = %100, %95
  br label %103

; <label>:103                                     ; preds = %102, %92
  %104 = load i32* @number_lines, align 4
  %105 = icmp ne i32 %104, 0
  br i1 %105, label %106, label %109

; <label>:106                                     ; preds = %103
  %107 = load i32* %3, align 4
  %108 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str25, i32 0, i32 0), i32 %107)
  br label %109

; <label>:109                                     ; preds = %106, %103
  store i32 2, i32* %pos, align 4
  br label %110

; <label>:110                                     ; preds = %109, %122
  br label %111

; <label>:111                                     ; preds = %110
  %112 = load %struct.str** %str, align 8
  %113 = getelementptr inbounds %struct.str* %112, i32 0, i32 0
  %114 = load i8** %113, align 8
  %115 = load i32* %pos, align 4
  %116 = sext i32 %115 to i64
  %117 = getelementptr inbounds i8* %114, i64 %116
  %118 = load i8* %117, align 1
  %119 = sext i8 %118 to i32
  %120 = icmp ne i32 %119, 32
  br i1 %120, label %122, label %121

; <label>:121                                     ; preds = %111
  br label %134

; <label>:122                                     ; preds = %111
  %123 = load %struct.str** %str, align 8
  %124 = getelementptr inbounds %struct.str* %123, i32 0, i32 0
  %125 = load i8** %124, align 8
  %126 = load i32* %pos, align 4
  %127 = sext i32 %126 to i64
  %128 = getelementptr inbounds i8* %125, i64 %127
  %129 = load i8* %128, align 1
  %130 = sext i8 %129 to i32
  %131 = call i32 @putchar(i32 %130)
  %132 = load i32* %pos, align 4
  %133 = add nsw i32 %132, 1
  store i32 %133, i32* %pos, align 4
  br label %110

; <label>:134                                     ; preds = %121
  %135 = call i32 @putchar(i32 10)
  br label %7

; <label>:136                                     ; preds = %88
  br label %137

; <label>:137                                     ; preds = %136, %80
  br label %138

; <label>:138                                     ; preds = %137
  br label %139

; <label>:139                                     ; preds = %138
  %140 = load %struct.str** %str, align 8
  %141 = getelementptr inbounds %struct.str* %140, i32 0, i32 0
  %142 = load i8** %141, align 8
  %143 = getelementptr inbounds i8* %142, i64 0
  %144 = load i8* %143, align 1
  %145 = sext i8 %144 to i32
  %146 = icmp eq i32 %145, 63
  br i1 %146, label %147, label %152

; <label>:147                                     ; preds = %139
  %148 = load i32* @verbose, align 4
  %149 = icmp ne i32 %148, 0
  br i1 %149, label %151, label %150

; <label>:150                                     ; preds = %147
  br label %7

; <label>:151                                     ; preds = %147
  br label %152

; <label>:152                                     ; preds = %151, %139
  %153 = load %struct.str** %str, align 8
  %154 = call i8* @str_to_nstr(%struct.str* %153)
  store i8* %154, i8** %tmp___1, align 8
  %155 = load i8** %tmp___1, align 8
  call void (i32, i32, i8*, ...)* @error___0(i32 0, i32 0, i8* getelementptr inbounds ([30 x i8]* @.str26, i32 0, i32 0), i8* %155)
  br label %6

; <label>:156                                     ; preds = %31
  ret void
}

declare i32 @fclose(%struct._IO_FILE*) #1

declare i32 @putchar(i32) #1

declare i32 @select(i32, %struct.__anonstruct_fd_set_13*, %struct.__anonstruct_fd_set_13*, %struct.__anonstruct_fd_set_13*, %struct.timeval*) #1

; Function Attrs: nounwind readonly
declare i32 @memcmp(i8*, i8*, i32) #5

declare i32 @fprintf(%struct._IO_FILE*, i8*, ...) #1

declare void (i32)* @signal(i32, void (i32)*) #1

; Function Attrs: nounwind uwtable
define internal void @sig_pipe(i32 %signo) #0 {
  %1 = alloca i32, align 4
  store i32 %signo, i32* %1, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([12 x i8]* @.str52, i32 0, i32 0))
  ret void
}

; Function Attrs: nounwind uwtable
define internal void @sig_chld(i32 %signo) #0 {
  %1 = alloca i32, align 4
  store i32 %signo, i32* %1, align 4
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([12 x i8]* @.str51, i32 0, i32 0))
  ret void
}

declare i32 @pipe(i32*) #1

declare i32 @close(i32) #1

; Function Attrs: nounwind readnone
declare i16** @__ctype_b_loc() #4

declare %struct._IO_FILE* @fopen(i8*, i8*) #1

declare i32 @dup2(i32, i32) #1

declare i32 @execl(i8*, i8*, ...) #1

; Function Attrs: nounwind uwtable
define internal i8* @xmalloc___0(i32 %size) #0 {
  %1 = alloca i32, align 4
  %ptr = alloca i8*, align 8
  %tmp = alloca i8*, align 8
  store i32 %size, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = call noalias i8* @malloc(i32 %2)
  store i8* %3, i8** %tmp, align 8
  %4 = load i8** %tmp, align 8
  store i8* %4, i8** %ptr, align 8
  %5 = load i8** %ptr, align 8
  %6 = icmp ne i8* %5, null
  br i1 %6, label %8, label %7

; <label>:7                                       ; preds = %0
  call void (i32, i32, i8*, ...)* @error___0(i32 1, i32 0, i8* getelementptr inbounds ([25 x i8]* @.str48, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i8** %ptr, align 8
  ret i8* %9
}

declare i8* @memcpy(i8*, i8*, i32) #1

declare noalias i8* @malloc(i32) #1

declare i32 @fflush(%struct._IO_FILE*) #1

; Function Attrs: nounwind
declare void @llvm.va_start(i8*) #6

declare i32 @vfprintf(%struct._IO_FILE*, i8*, %struct.__va_list_tag*) #1

; Function Attrs: nounwind
declare void @llvm.va_end(i8*) #6

declare i8* @strerror(i32) #1

declare i32 @_IO_putc(i32, %struct._IO_FILE*) #1

declare i32 @read(...) #1

declare noalias i8* @realloc(i8*, i32) #1

; Function Attrs: nounwind uwtable
define internal void @error(i32 %status, i32 %errnum, i8* %message, ...) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8*, align 8
  %args = alloca [1 x %struct.__va_list_tag], align 16
  %tmp = alloca i8*, align 8
  store i32 %status, i32* %1, align 4
  store i32 %errnum, i32* %2, align 4
  store i8* %message, i8** %3, align 8
  %4 = load i32* @interactive, align 4
  %5 = icmp ne i32 %4, 0
  br i1 %5, label %12, label %6

; <label>:6                                       ; preds = %0
  %7 = load %struct._IO_FILE** @stdout, align 8
  %8 = call i32 @fflush(%struct._IO_FILE* %7)
  %9 = load %struct._IO_FILE** @stderr, align 8
  %10 = load i8** @program_name, align 8
  %11 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %9, i8* getelementptr inbounds ([5 x i8]* @.str49, i32 0, i32 0), i8* %10)
  br label %12

; <label>:12                                      ; preds = %6, %0
  %13 = getelementptr inbounds [1 x %struct.__va_list_tag]* %args, i32 0, i32 0
  %14 = bitcast %struct.__va_list_tag* %13 to i8*
  call void @llvm.va_start(i8* %14)
  %15 = load %struct._IO_FILE** @stderr, align 8
  %16 = load i8** %3, align 8
  %17 = getelementptr inbounds [1 x %struct.__va_list_tag]* %args, i32 0, i32 0
  %18 = call i32 @vfprintf(%struct._IO_FILE* %15, i8* %16, %struct.__va_list_tag* %17)
  %19 = getelementptr inbounds [1 x %struct.__va_list_tag]* %args, i32 0, i32 0
  %20 = bitcast %struct.__va_list_tag* %19 to i8*
  call void @llvm.va_end(i8* %20)
  %21 = load i32* %2, align 4
  %22 = icmp ne i32 %21, 0
  br i1 %22, label %23, label %29

; <label>:23                                      ; preds = %12
  %24 = load i32* %2, align 4
  %25 = call i8* @strerror(i32 %24)
  store i8* %25, i8** %tmp, align 8
  %26 = load %struct._IO_FILE** @stderr, align 8
  %27 = load i8** %tmp, align 8
  %28 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %26, i8* getelementptr inbounds ([5 x i8]* @.str50, i32 0, i32 0), i8* %27)
  br label %29

; <label>:29                                      ; preds = %23, %12
  %30 = load %struct._IO_FILE** @stderr, align 8
  %31 = call i32 @_IO_putc(i32 10, %struct._IO_FILE* %30)
  %32 = load %struct._IO_FILE** @stderr, align 8
  %33 = call i32 @fflush(%struct._IO_FILE* %32)
  %34 = load i32* %1, align 4
  %35 = icmp ne i32 %34, 0
  br i1 %35, label %36, label %38

; <label>:36                                      ; preds = %29
  %37 = load i32* %1, align 4
  call void @exit(i32 %37) #8
  unreachable

; <label>:38                                      ; preds = %29
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { inlinehint nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind readonly "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nounwind }
attributes #7 = { nounwind readnone }
attributes #8 = { noreturn }
attributes #9 = { nounwind readonly }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Ubuntu clang version 3.4-1ubuntu3 (tags/RELEASE_34/final) (based on LLVM 3.4)"}
