//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#include <llvm/Support/CommandLine.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/IR/LLVMContext.h>
#include "llvm/IR/InstIterator.h"
#include <llvm/IR/Instructions.h>
#include "llvm/IR/DebugInfo.h"
#include "llvm/Pass.h"
#include <llvm/Support/SourceMgr.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/ToolOutputFile.h>

#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>

#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#include <iostream>

#ifdef _DEBUG
#define LINE cnt
#else
#define LINE I.getDebugLoc().getLine()
#endif

using namespace llvm;
static ManagedStatic<LLVMContext> GlobalContext;
static LLVMContext &getGlobalContext() { return *GlobalContext; }
/* In LLVM 5.0, when  -O0 passed to clang , the functions generated with clang will
 * have optnone attribute which would lead to some transform passes disabled, like mem2reg.
 */
struct EnableFunctionOptPass: public FunctionPass {
    static char ID;
    EnableFunctionOptPass():FunctionPass(ID){}
    bool runOnFunction(Function & F) override{
        if(F.hasFnAttribute(Attribute::OptimizeNone))
        {
            F.removeFnAttr(Attribute::OptimizeNone);
        }
        return true;
    }
};

char EnableFunctionOptPass::ID=0;

	
///!TODO TO BE COMPLETED BY YOU FOR ASSIGNMENT 2
///Updated 11/10/2017 by fargo: make all functions
///processed by mem2reg before this pass.
struct FuncPtrPass : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  FuncPtrPass() : ModulePass(ID) {}

  bool isIndirect = false;

  std::map<unsigned, std::set<std::string>> lineToFunctionsMap;

    /// PHINode 处理函数，可能有递归
  /// https://llvm.org/doxygen/classllvm_1_1PHINode.html
  void handlePHINode(const PHINode *phiNode, int line) {
      // 遍历PHINode的所有可能的前驱值
      for (unsigned i = 0; i < phiNode->getNumIncomingValues(); ++i) {
          Value *incomingValue = phiNode->getIncomingValue(i);
          handleValue(incomingValue, line);
      }
  }

  void handleReturn(const ReturnInst* ret, int line){
      handleValue(ret->getReturnValue(), line);
  }


  // 处理 Argument 类型的 Value
  // 主要用刀了 Use 和 User 两个类
  void handleArgument(const Argument* arg, int line){
    const unsigned int argIdx = arg->getArgNo(); // 形参在函数参数中的位置
    // 获取该参数所在函数的所有调用者
    const Function* parent = arg->getParent();
    for(const User* user: parent->users()){
        // 获取参数所在函数的调用
        if (const CallInst *callInst = dyn_cast<CallInst>(user)) {
            //user->dump();
            // pasted from https://github.com/ChinaNuke/llvm-pass/blob/master/LLVMAssignment.cpp
            const Function *calledFunction = callInst->getCalledFunction();
            if (calledFunction == parent) {
                const Value *operand = callInst->getArgOperand(argIdx);
                /// test17.ll 这里会直接跳到handleValue之后handleFunc
                //if(auto* func = dyn_cast<Function>(operand)){
                    // 这里还需要额外操作。

                //} else {
                    handleValue(operand,line);
                //}
            } else {
                for (const BasicBlock &bb : *calledFunction) {
                    for (const Instruction &i : bb) {
                        if (const ReturnInst *retInst = dyn_cast<ReturnInst>(&i)) {
                            const Value *retValue = retInst->getReturnValue();
                            if (const Argument *arg = dyn_cast<Argument>(retValue)) {
                                handleArgument(arg, line);
                            } else if (const CallInst *callInst = dyn_cast<CallInst>(retValue)) {
                                const Value *operand = callInst->getArgOperand(argIdx);
                                if (const Argument *arg = dyn_cast<Argument>(operand)) {
                                    handleArgument(arg, line);
                                } else {
                                }
                            } else {
                            }

                        }
                    }
                }
            }
//            Value *operand = callInst->getArgOperand(argIdx);
//            handleValue(operand, line);
        } else if (const PHINode *phiNode = dyn_cast<PHINode>(user)) {
            for (const User *phiUser: phiNode->users()) {
                if (const CallInst *outerCallInst = dyn_cast<CallInst>(phiUser)) {
                    Value *operand = outerCallInst->getArgOperand(argIdx);
                    handleValue(operand, line);
                }
            }
        }else {
            //errs() << "Unhandled User: ";
            //user->dump();
        }
    }
  }

  /// test04.ll : i32 (i32, i32)* %a_fptr 是参数类型
  /// 需要添加处理函数参数的代码
  /// test11.ll ： %call = call i32 (i32, i32)* @foo(i32 %a, i32 %b, i32 (i32, i32)* %a_fptr, i32 (i32, i32)* %b_fptr), !dbg !19
  /// 需要考虑函数返回值为函数的情况
  void handleValue(const Value* value, int line){
      if(auto* phiNode = dyn_cast<PHINode>(value)){
          handlePHINode(phiNode, line);
      } else if(auto* argument = dyn_cast<Argument>(value)) {
          handleArgument(argument, line);
      } else if(auto* call = dyn_cast<CallInst>(value)) {
          /// test11.ll 主要修改这个 if 里的逻辑
          /// 需要支持函数返回值为函数指针的情况
          /// 比如 ret i32 (i32, i32)* %call, !dbg !20 里 getReturnVal 获得的就是 %call 他是一个函数指针
          /// 不应该放在 handleValue 里做
//          if (call->getType()->isPointerTy() &&
//              call->getType()->getPointerElementType()->isFunctionTy()) {
//              handleFuncPtrRet(call, line);
//          } else {
//              handleCall(call, line);
//          }
          handleCall(call, line);
      } else if (const ReturnInst *returnInst = dyn_cast<ReturnInst>(value)) {
          handleReturn(returnInst, line);
      } else if(auto* func = dyn_cast<Function>(value)) {
          handleFunc(func, line);
      //} else if(auto* ret = dyn_cast<ReturnInst>(value)) {
     //     handleRet(ret, line);
      } else {
//          errs() << "Unsupport value: ";
//          value->dump();
      }
  }

  /// test11.ll 函数指针为一个函数返回值的情况，否则如果进入到 handleCall 会获得错误结果。
  /// 为什么不用遍历 User 来处理，而是遍历嵌套 CallInst ：
  /// 函数指针被返回并立即用于另一个 CallInst，分析应该跟踪到这个立即的 CallInst。
  /// test16.ll should output minus but get plus
  /// 我怎么觉得就是 plus 呢，答案是不是错了
  /// 该函数整合到 handleCall 里比较合适，因为他关注的是 %call 这类的指针而不是具体的return语句
  void handleFuncPtrRet(const CallInst* call, int line){
      const Value* operand = call->getCalledOperand();
      if (const Function *calledFunc = call->getCalledFunction()) {
          // CallInst 的内部嵌套 CallInst，说明实际调用的是 Inner CallInst
          // 的返回值，而不是 Inner CallInst 自身。
          for (const BasicBlock &bb : *calledFunc) {
              for (const Instruction &i : bb) {
                  if (const ReturnInst *retInst = dyn_cast<ReturnInst>(&i)) {
                      const Value *retValue = retInst->getReturnValue();
//                      errs() << "handled funcPtrRet:::";
//                      i.dump();
                      handleValue(retValue, line);
//                  } else {
//                      errs() << "unhandled funcPtrRet:";
//                      i.dump();
                  }
              }
          }
      } else if(const CallInst* innerCall = dyn_cast<CallInst>(operand)){
          /// test17.ll 多层嵌套, 没有触发
//          errs() << "多层嵌套触发";
      } else {
          /// test17.ll
          /// %call = call i32 (i32, i32)* %goo_ptr(i32 %a, i32 %b, i32 (i32, i32)* %b_fptr, i32 (i32, i32)* %a_fptr), !dbg !23
//          errs() << "handleFuncPtrRet unhandled:" ;
//          call->dump();
//          errs() << "Operand: ";
//          operand->dump();
          // test17.ll 在这里分析 argument ?
          handleValue(operand, line);
      }

      return ;
      // Iterate over all the users of this function pointer
      for (auto* user : call->users()) {
          if (isa<CallInst>(user)) {
              if(auto* userCallInst = dyn_cast<CallInst>(user)) {
                  // If the user is a CallInst, it may be a function call using the pointer
                  handleCall(userCallInst, line);
              }
//          } else if (auto* storeInst = dyn_cast<StoreInst>(user)) {
//              // If the user is a StoreInst, the function pointer is stored somewhere
//              const Value* storedValue = storeInst->getValueOperand();
//              // Continue to track where this stored value is used
//              trackStoredFunctionPointer(storedValue, line);
          }
          // ... handle other types of users
      }
  }



  void handleFunc(const Function* func, int line){
      // Ignore intrinsic functions, return true when function start with llvm
      if (!func->isIntrinsic()) {
          // 这里获取不到在 if 里面赋值的函数指针，需要处理 PHINode
          //func->dump();
          /// 这里不能直接加入到结果集，他有可能执行的是返回的函数？
          auto* type = func->getReturnType();
          if(isIndirect && type->isPointerTy() && type->getPointerElementType()->isFunctionTy()){
              /// test17.ll
              /// 这样写会导致结果全部变成了plus，但是25行的实际调用确实是plus
              /// 需要做一个额外区分。
              for (const BasicBlock &bb : *func) {
                  for (const Instruction &i : bb) {
                      if (const ReturnInst *retInst = dyn_cast<ReturnInst>(&i)) {
                          const Value *retValue = retInst->getReturnValue();
//                          errs() << "handled funcPtrRet:::";
//                          i.dump();
                          handleValue(retValue, line);
                      }
                  }
              }
          } else {
              lineToFunctionsMap[line].insert(func->getName().str());
          }
          isIndirect = false;
          /// test13.ll call 里面又有其他的call，比如
          /// %call = call i32 (i32, i32)* %2(i32 %3, i32 %4, i32 (i32, i32)* %5, i32 (i32, i32)* %6), !dbg !86
      }
  }

  /// test13.ll
  void handleFuntionReturnStatement(const Function* func, int line){
      for (const BasicBlock &bb : *func) {
          for (const Instruction &i : bb) {
              if (const ReturnInst *retInst = dyn_cast<ReturnInst>(&i)) {
                  const Value *retValue = retInst->getReturnValue();
//                  errs() << "handle Ret: ";
//                  retInst->dump();
                  handleValue(retValue, line);
              }
          }
      }
  }

  /// 核心函数
  /// 独立出 handleCall 处理 call 嵌套的问题。
  /// test11.ll 这里 handleCall 会输出错误的结果，猜测是返回值的问题
  /// handleCall -> handleValue -> handleCall
  void handleCall(const CallInst* call, int line){
      // 可以直接换成 Function 的
      /// test16.ll: %call1 = call i32 %call(i32 %op1, i32 %op2), !dbg !27
      if (Function *calledFunction = call->getCalledFunction()) {
          handleFunc(calledFunction, line);
      } else { // 不可以直接换成 Function 的，就必须分析Operand进一步分析
          // 获取操作数
          /// test11.ll 流程：
          /// 1. 先获取 Call，发现不是直接调用
          /// 2. 把 Operand 传递给了 handleValue
          /// 3. handleValue 发现 Operand 是一个直接调用
          /// 4. 记录答案
          /// 错在了 Operand 虽然是一个直接调用，但是调用的是 foo 返回的函数指针 plus。
          /// test13.ll 这里不能直接调用 handleValue，
          /// handleValue 识别不了嵌套CallInst 的情况。
          /// 这里调用 handleValue 无限递归。
          const Value *operand = call->getCalledOperand();
          /// call的 operand 仍然是一个 call，这里可以认为是调用了函数指针，需要分析函数指针的值。j
          if (const CallInst *innerCallInst = dyn_cast<CallInst>(operand)) {
              isIndirect = true;
              /// test16.ll: 嵌套 Call 的情况:   %call1 = call i32 %call(i32 %op1, i32 %op2), !dbg !27
//              errs() << "嵌套 Call 的情况: ";
//              call->dump();
//              errs() << "inner: ";
//              innerCallInst->dump();
//              errs() << "\n";

              /// 内部call可以直接获取函数
              if (const Function *calledFunc = innerCallInst->getCalledFunction()) {
                  /// test17.ll inner:
                  /// %call = call i32 (i32, i32)* @clever1(i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* @clever, i32 %op1, i32 %op2, i32 (i32, i32)* @minus, i32 (i32, i32)* @plus), !dbg !25
                  /// test17.ll 34行的IR handleFunctionReturnStatement->handleValue->handleFuncPtrRet(这里直接if为false) 结束
                  /// 实际上 test17.ll 不仅仅套了一层，而是套了很多层，所以需要在handleFuncPtrRet补全if else
                  handleFuntionReturnStatement(calledFunc, line);
              } else {
                  // 这里是 innerCall 又是间接调用的情况
                  const Value *innerCallInstOperand = innerCallInst->getCalledOperand();
                  if (const PHINode *phiNode =
                          dyn_cast<PHINode>(innerCallInstOperand)) {
                      for (const Value *income_func : phiNode->incoming_values()) {
                          if (const Function *calledFunc =
                                  dyn_cast<Function>(income_func)) {
                              handleFuntionReturnStatement(calledFunc, line);
                          }
                      }
                  }
              }
          } else if (const PHINode *phiNode = dyn_cast<PHINode>(operand)) {
              handlePHINode(phiNode, line);
          } else if (const Argument *arg = dyn_cast<Argument>(operand)) {
              handleArgument(arg, line);
          } else {
              //operand->dump();
          }

//          handleValue(operand, line);
      }
  }

  /// test14.ll 会多一个 32: minuns 的输出
  bool runOnModule(Module &M) override {
      int cnt = 0;
      for (Function &F : M) {
          for (BasicBlock &BB : F) {
              for (Instruction &I : BB) {
                  /// test10.ll 少这一行
                  /// 18  %s_fptr.0 = phi i32 (i32, i32)* [ %a_fptr, %if.then ], [ %b_fptr, %if.else ], !dbg !24

//                  errs() << cnt ++;
//                  I.dump();

                  // 由于是打印调用的函数名，所以只处理 Call
                  if (auto *callInst = dyn_cast<CallInst>(&I)) {
                      handleCall(callInst, I.getDebugLoc().getLine());
                  }
              }
          }
      }

      // 打印结果
      for (const auto &entry : lineToFunctionsMap) {
          errs() << entry.first << ": ";
          for (const auto &funcName : entry.second) {
              errs() << funcName << ", ";
          }
          errs() << "\n";
      }

      return false;
  }
};


char FuncPtrPass::ID = 0;
static RegisterPass<FuncPtrPass> X("funcptrpass", "Print function call instruction");

static cl::opt<std::string>
InputFilename(cl::Positional,
              cl::desc("<filename>.bc"),
              cl::init(""));


int main(int argc, char **argv) {
    const char *c[2];
    std::string s("/root/assign2/bc/test");
    if (argc == 1) {
        std::string t;
        std::cout << "请输入测试编号：";
        std::cin >> t;
        s.append(t).append(".ll");
        c[1] = s.c_str();
        // Parse the command line to read the Inputfilename
        cl::ParseCommandLineOptions(2, c,
                                    "FuncPtrPass \n My first LLVM too which does not do much.\n");
    } else {
        // Parse the command line to read the Inputfilename
        cl::ParseCommandLineOptions(argc, argv,
                                    "FuncPtrPass \n My first LLVM too which does not do much.\n");
    }

   LLVMContext &Context = getGlobalContext();
   SMDiagnostic Err;

   // Load the input module
   std::unique_ptr<Module> M = parseIRFile(InputFilename, Err, Context);
   if (!M) {
      Err.print(argv[0], errs());
      return 1;
   }

   llvm::legacy::PassManager Passes;
   	
   ///Remove functions' optnone attribute in LLVM5.0
   Passes.add(new EnableFunctionOptPass());
   ///Transform it to SSA
   Passes.add(llvm::createPromoteMemoryToRegisterPass());

   /// Your pass to print Function and Call Instructions
   Passes.add(new FuncPtrPass());
   Passes.run(*M.get());
}

