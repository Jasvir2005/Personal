import axios from "axios";
import { useState } from "react";
import { useNavigate } from "react-router-dom";

export default function VerifyOtp() {

  const [otp, setOtp] =
    useState("");

  const nav = useNavigate();

  const email =
    localStorage.getItem("resetEmail");

  const verifyOtp = async () => {

    try {

      await axios.post(
        "http://localhost:5000/auth/verify-otp",
        {
          email,
          otp
        }
      );

      nav("/reset-password");

    } catch (err) {

      alert("Wrong OTP");

    }

  };

  return (

    <div className="auth-container">

      <div className="auth-box">

        <h2>Verify OTP</h2>

        <input
          placeholder="Enter OTP"
          onChange={(e) =>
            setOtp(e.target.value)
          }
        />

        <button onClick={verifyOtp}>
          Verify
        </button>

      </div>

    </div>

  );

}