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

  std::map<unsigned, std::set<std::string>> lineToFunctionsMap;

    /// PHINode 处理函数，可能有递归
  /// https://llvm.org/doxygen/classllvm_1_1PHINode.html
  void handlePHINode(const PHINode *phiNode, int line) {
      // 遍历PHINode的所有可能的前驱值
      for (unsigned i = 0; i < phiNode->getNumIncomingValues(); ++i) {
          Value *incomingValue = phiNode->getIncomingValue(i);
          BasicBlock *incomingBlock = phiNode->getIncomingBlock(i);

          // 处理每个前驱值
          if (auto *incomingPHINode = dyn_cast<PHINode>(incomingValue)) {
              // 如果前驱值是另一个PHINode，递归处理
              handlePHINode(incomingPHINode, line);
          } else if (auto *inst = dyn_cast<Instruction>(incomingValue)) {
              // 如果前驱值是一个指令，处理这个指令
              // ...
          } else if (auto *arg = dyn_cast<Argument>(incomingValue)) {
              // 如果前驱值是一个参数，处理这个参数
              // ...
          } else if (auto *func = dyn_cast<Function>(incomingValue)){
              // 前驱是一个函数
              lineToFunctionsMap[line].insert(func->getName());
          }
      }
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
            Value *operand = callInst->getArgOperand(argIdx);
            handleValue(operand, line);
        } else {
            errs() << "Unhandled user of parent function of argument: ";
            user->dump();
        }
    }
  }

  void handleValue(const Value* value, int line){
      if(auto* phiNode = dyn_cast<PHINode>(value)){
          handlePHINode(phiNode, line);
      } else if(auto* argument = dyn_cast<Argument>(value)) {
          handleArgument(argument, line);
      }else {
          /// test04.ll : i32 (i32, i32)* %a_fptr 是参数类型
          errs() << "Unsupport format : ";
          value->dump();
      }
  }

  bool runOnModule(Module &M) override {

      for (Function &F : M) {
          for (BasicBlock &BB : F) {
              for (Instruction &I : BB) {
                  if (auto *callInst = dyn_cast<CallInst>(&I)) {
                      // 可以直接换成 Function 的
                      if (Function *calledFunction = callInst->getCalledFunction()) {
                          // Ignore intrinsic functions, return true when function start with llvm
                           if (!calledFunction->isIntrinsic()) {
                              // 这里获取不到在 if 里面赋值的函数指针，需要处理 PHINode
                              if (const DebugLoc &debugInfo = I.getDebugLoc()) { // Here the debug information is obtained
                                  unsigned line = debugInfo.getLine();
                                  lineToFunctionsMap[line].insert(calledFunction->getName().str());
                              }
                           }
                      } else { // 不可以直接换成 Function 的
                          // 获取操作数
                          const Value *value = callInst->getCalledOperand();
                          handleValue(value, I.getDebugLoc().getLine());
//                          if(auto* phiNode = dyn_cast<PHINode>(value)){
//                              handlePHINode(phiNode, I.getDebugLoc().getLine());
//                          } else if(auto* argument = dyn_cast<Argument>(value)) {
//                              handleArgument(argument, I.getDebugLoc().getLine());
//                          }else {
//                              /// test04.ll : i32 (i32, i32)* %a_fptr 是参数类型
//                              errs() << "Unsupport format : ";
//                              value->dump();
//                          }

                      }
                  }
              }
          }
      }

      // Printing the gathered information
      for (const auto &entry : lineToFunctionsMap) {
          errs() << entry.first << " : ";
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

