# 🚀 Decentralized Job Board with Crypto Payments

A trustless platform for posting and completing jobs with guaranteed crypto payments through smart contracts.

## 🎯 Features

- 💼 Post jobs with locked STX payments
- 👥 Apply for available jobs
- ✅ Accept applications and assign workers
- 💰 Automatic payment release upon completion
- ⭐ Built-in reputation system

## 📝 Contract Functions

### For Employers

- `create-job`: Post a new job with title, description, and locked payment
- `accept-application`: Choose a worker for your job

### For Workers

- `apply-for-job`: Submit application for open jobs
- `complete-job`: Mark job as completed to receive payment

### Read-Only Functions

- `get-job`: View details of a specific job
- `get-user-reputation`: Check reputation score of any user

## 🔧 Usage

1. Deploy the contract to the Stacks blockchain
2. Employers: Create jobs by calling `create-job` with STX payment
3. Workers: Browse jobs and apply using `apply-for-job`
4. Complete work and receive automatic payment

## 🔒 Security

- Funds are locked in contract until job completion
- Only authorized users can perform sensitive actions
- Reputation system tracks successful completions
```

